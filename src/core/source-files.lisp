(in-package #:autolith)

;;;; -- Source File Discovery --

(defparameter *project-lisp-source-directories*
  '("src/" "nooa-autolith-extension/")
  "Repository-relative directories of Lisp compiled into the Autolith image.")

(-> source-lisp-pathnames (pathname) list)
(defun source-lisp-pathnames (directory)
  "Return sorted Lisp source pathnames recursively beneath DIRECTORY."
  (labels ((collect-pathnames (current-directory)
             "Return Lisp files beneath CURRENT-DIRECTORY in arbitrary order."
             (append
              (uiop:directory-files current-directory "*.lisp")
              (mapcan #'collect-pathnames
                      (uiop:subdirectories current-directory)))))
    (sort (collect-pathnames (uiop:ensure-directory-pathname directory))
          #'string<
          :key #'namestring)))

(-> project-lisp-pathnames (pathname) list)
(defun project-lisp-pathnames (source-root)
  "Return sorted Lisp pathnames compiled into Autolith from SOURCE-ROOT."
  (sort
   (mapcan
    (lambda (relative)
      (let ((directory
              (merge-pathnames
               relative
               (uiop:ensure-directory-pathname source-root))))
        (if (uiop:directory-exists-p directory)
            (source-lisp-pathnames directory)
            nil)))
    *project-lisp-source-directories*)
   #'string<
   :key #'namestring))
