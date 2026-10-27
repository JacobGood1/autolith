;;;; -- Source Check Dependencies --

;;; cheque, the source checker, Mallet, and the libraries only they need are
;;; development tools, like Cargo's dev-dependencies. checks/qlfile lists them
;;; apart from the runtime qlfile and only the check scripts install or see
;;; them, so releases, Nix packages and running Autolith images never carry
;;; them. They install as a qlot project in Autolith's cache, outside the
;;; source tree, because ASDF's default source registry searches trees like
;;; ~/common-lisp and would otherwise find Mallet's vendored ASDF below a
;;; checkout.

(require :sb-md5)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *autolith-check-dependency-files*
  '("qlfile" "qlfile.lock" "autolith-check-tools.asd")
  "The checks/ files that define the check tools' qlot project.")

(defvar *autolith-check-dependency-directories* nil
  "The installed check tool source directories ASDF consults last.")

(defun autolith-check-dependency-root (source-root)
  "Return the cached qlot project for SOURCE-ROOT's current check tool definition.

Its name is a digest of the defining files, so a changed lock installs afresh."
  (let ((digest (sb-md5:md5sum-string
                 (with-output-to-string (stream)
                   (dolist (name *autolith-check-dependency-files*)
                     (write-string (uiop:read-file-string
                                    (merge-pathnames name (merge-pathnames "checks/" source-root)))
                                   stream)
                     (write-char #\Nul stream)))
                 :external-format ':utf-8)))
    (merge-pathnames (format nil "check-tools/~(~{~2,'0x~}~)/" (coerce digest 'list))
                     (autolith-application-root :cache))))

(defun autolith-check-dependency-stamp (source-root)
  "Return the file recording a complete install of SOURCE-ROOT's check tools."
  (merge-pathnames "installed" (autolith-check-dependency-root source-root)))

(defun autolith-ensure-check-dependencies (source-root script-command)
  "Install SOURCE-ROOT's locked check tools unless they are already installed.

SCRIPT-COMMAND is the command prefix that runs one Lisp script file."
  (let ((root (autolith-check-dependency-root source-root)))
    (unless (probe-file (autolith-check-dependency-stamp source-root))
      (format t "~&Installing the locked source-check tools.~%")
      (finish-output)
      (dolist (name *autolith-check-dependency-files*)
        (uiop:copy-file (merge-pathnames name (merge-pathnames "checks/" source-root))
                        (ensure-directories-exist (merge-pathnames name root))))
      (uiop:run-program (append script-command
                                (list (uiop:native-namestring
                                       (merge-pathnames "script/qlot-install.lisp" source-root))
                                      (uiop:native-namestring root)))
                        :output       ':interactive
                        :error-output ':interactive)
      (with-open-file (stream (autolith-check-dependency-stamp source-root)
                              :direction ':output :if-exists ':supersede)
        (write-line "installed" stream)))))

(defun autolith-check-dependency-directories (source-root)
  "Return every installed check tool source directory for SOURCE-ROOT."
  (let ((dists (merge-pathnames ".qlot/dists/" (autolith-check-dependency-root source-root))))
    (loop for dist in (uiop:subdirectories dists)
          append (uiop:subdirectories (merge-pathnames "software/" dist)))))

(defun autolith-find-check-system-definition (name directory &optional (depth 2))
  "Return NAME's system definition in DIRECTORY or its visible subdirectories to DEPTH.

Dot directories, such as Mallet's vendored libraries, are never searched."
  (or (probe-file (merge-pathnames (make-pathname :name name :type "asd") directory))
      (when (plusp depth)
        (some (lambda (subdirectory)
                (let ((leaf (first (last (pathname-directory subdirectory)))))
                  (unless (and (stringp leaf) (plusp (length leaf)) (char= (char leaf 0) #\.))
                    (autolith-find-check-system-definition name subdirectory (1- depth)))))
              (uiop:subdirectories directory)))))

(defun autolith-locate-check-system (name)
  "Locate NAME among the check tools when no other ASDF search found it.

Running after every other search, it never shadows Autolith or the locked
runtime libraries with the tools' own copies."
  (let ((primary (asdf:primary-system-name (string-downcase (string name)))))
    (unless (asdf:registered-system primary)
      (loop for directory in *autolith-check-dependency-directories*
              thereis (autolith-find-check-system-definition primary directory)))))

(defun autolith-register-check-dependencies (source-root)
  "Make SOURCE-ROOT's installed check tools loadable after the runtime registry."
  (setf *autolith-check-dependency-directories*
        (autolith-check-dependency-directories source-root))
  (unless (member 'autolith-locate-check-system asdf:*system-definition-search-functions*)
    (setf asdf:*system-definition-search-functions*
          (append asdf:*system-definition-search-functions*
                  (list 'autolith-locate-check-system)))))
