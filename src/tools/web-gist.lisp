(in-package #:autolith)

;;;; -- Standalone Web Page Retrieval --

(defclass web-gist-tool (tool)
  ()
  (:documentation
   "A tool that fetches one web page and returns it as Markdown."))

(-> web-gist-parameters () json-object)
(defun web-gist-parameters ()
  "Return the web_extra.gist parameter schema."
  (tool-object-schema
   (json-object
    "url" (tool-string-property
           "The absolute HTTP or HTTPS URL of the page to retrieve."))
   '("url")))

(-> web-gist--retrieve (string) string)
(defun web-gist--retrieve (url)
  "Fetch URL and return its Markdown representation."
  (let ((uri (quri:uri url)))
    (unless (and (member (quri:uri-scheme uri) '("http" "https")
                         :test #'string=)
                 (non-empty-string-p (quri:uri-host uri)))
      (error 'tool-error
             :message "web_extra.gist requires an absolute HTTP or HTTPS URL with a host."
             :tool-name "web_extra.gist")))
  (fetch-gist:markdown-from-url url))

(defmethod tool-execute ((tool web-gist-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Fetch one web page and retain a receipt identifying its source, result, and retrieval time."
  (declare (ignore tool))
  (let ((url (tool-argument arguments "url" :required t)))
    (unless (non-empty-string-p url)
      (error 'tool-error
             :message "web_extra.gist requires a non-empty string url."
             :tool-name "web_extra.gist"))
    (handler-case
        (let ((output (web-gist--retrieve url)))
          (when (and (tool-context-conversation context)
                     (tool-context-call-id context)
                     (knowledge-capture-p (tool-context-configuration context)))
            (conversation-append-record
             (tool-context-conversation context)
             (list :web-retrieval
                   :tool "web_extra.gist"
                   :url url
                   :call-id (tool-context-call-id context)
                   :result-digest (memory-source-text-digest output)
                   :observed-at (knowledge-now))))
          (tool-success output))
      (error (condition)
        (error 'tool-error
               :message (format nil "web_extra.gist could not retrieve ~A: ~A"
                                url condition)
               :tool-name "web_extra.gist")))))
