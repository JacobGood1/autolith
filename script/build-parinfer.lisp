;;;; -- Private Parinfer Library --

;;; The source checks run parinfer-rust's paren mode exactly as editors do.
;;; This builds the C library of the fork parinfer-rust-mode downloads, at the
;;; commit native/parinfer/commit pins, without its Emacs module, and installs
;;; it under the per-user data root. Its own Cargo.lock pins the dependencies.

(require :asdf)
(require :sb-posix)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defun parinfer-build--run (command &key directory)
  "Run one build COMMAND in DIRECTORY, preserving its output."
  (uiop:run-program command
                    :directory    directory
                    :input        ':interactive
                    :output       ':interactive
                    :error-output ':interactive))

(defun parinfer-build--manifest-current-p (library manifest commit)
  "Return true when the installed LIBRARY's MANIFEST records COMMIT."
  (and (probe-file library)
       (probe-file manifest)
       (handler-case
           (with-open-file (stream manifest :direction ':input :external-format ':utf-8)
             (let ((*read-eval* nil))
               (equal (read stream nil nil)
                      (list :parinfer-library :version 1 :commit commit))))
         (error ()
           nil))))

(defun parinfer-build--publish (source target)
  "Atomically copy SOURCE over TARGET."
  (let ((temporary (make-pathname :name     (format nil ".~A.~D" (pathname-name target)
                                                    (sb-posix:getpid))
                                  :type     (pathname-type target)
                                  :defaults target)))
    (unwind-protect
         (progn
           (uiop:copy-file source temporary)
           (uiop:rename-file-overwriting-target temporary target))
      (when (probe-file temporary)
        (delete-file temporary)))))

(defun parinfer-build--write-manifest (manifest commit)
  "Atomically record that the installed library was built from COMMIT."
  (let ((temporary (make-pathname :name     (format nil ".manifest.~D" (sb-posix:getpid))
                                  :type     "sexp"
                                  :defaults manifest)))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction ':output :if-exists ':supersede
                                             :external-format ':utf-8)
             (prin1 (list :parinfer-library :version 1 :commit commit) stream)
             (terpri stream))
           (uiop:rename-file-overwriting-target temporary manifest))
      (when (probe-file temporary)
        (delete-file temporary)))))

(let* ((source-root  (uiop:pathname-parent-directory-pathname
                      (uiop:pathname-directory-pathname (truename *load-truename*))))
       (commit       (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  (uiop:read-file-string
                                   (merge-pathnames "native/parinfer/commit" source-root))))
       (checkout     (merge-pathnames (format nil "build/parinfer/~A/" commit)
                                      (autolith-application-root :cache)))
       (install-root (merge-pathnames "native/parinfer/" (autolith-application-root :data)))
       (library-name #+darwin "libparinfer_rust.dylib"
                     #+win32 "parinfer_rust.dll"
                     #-(or darwin win32) "libparinfer_rust.so")
       (library      (merge-pathnames library-name install-root))
       (manifest     (merge-pathnames "manifest.sexp" install-root)))
  (if (parinfer-build--manifest-current-p library manifest commit)
      (format t "~&The pinned private parinfer library is already installed at ~A.~%" library)
      (progn
        (unless (probe-file (merge-pathnames ".git/" checkout))
          (ensure-directories-exist checkout)
          (parinfer-build--run (list "git" "init" (namestring checkout)))
          (parinfer-build--run (list "git" "-C" (namestring checkout) "remote" "add" "origin"
                                     "https://github.com/justinbarclay/parinfer-rust-emacs.git")))
        (format t "~&Fetching parinfer-rust at ~A.~%" commit)
        (finish-output)
        (parinfer-build--run (list "git" "-C" (namestring checkout)
                                   "fetch" "--depth" "1" "origin" commit))
        (parinfer-build--run (list "git" "-C" (namestring checkout)
                                   "checkout" "--detach" "--force" "FETCH_HEAD"))
        (let ((actual (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (uiop:run-program
                                    (list "git" "-C" (namestring checkout) "rev-parse" "HEAD")
                                    :output ':string))))
          (unless (string= actual commit)
            (error "Fetched parinfer-rust commit ~A instead of ~A." actual commit)))
        ;; The release's lock file predates its own version bump, so Cargo
        ;; rewrites that one entry; every dependency stays as locked.
        (parinfer-build--run (list "cargo" "build" "--release" "--lib" "--no-default-features")
                             :directory checkout)
        (let ((built (merge-pathnames (format nil "target/release/~A" library-name) checkout)))
          (unless (probe-file built)
            (error "parinfer-rust did not produce ~A." built))
          (ensure-directories-exist library)
          (parinfer-build--publish built library)
          (parinfer-build--write-manifest manifest commit)
          (format t "~&Installed the private parinfer library at ~A.~%" library)))))
