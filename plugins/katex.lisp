(defpackage :coleslaw-katex
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:index
                          #:tag-p
                          #:index-content))

(in-package :coleslaw-katex)

(defvar *katex-header* "
<link rel=\"stylesheet\" href=\"~a/katex.min.css\" />
<script defer src=\"~a/katex.min.js\"></script>
<script defer src=\"~a/contrib/auto-render.min.js\" onload=\"renderMathInElement(document.body);\"></script>
")

(defgeneric katex-p (document)
  (:documentation "Test if DOCUMENT requires contains any math-tagged content.")
  (:method ((content content))
    (tag-p "math" content))
  (:method ((index index))
    (and (slot-boundp index 'content)
         (some #'katex-p (index-content index)))))

(defun enable (&key force config (location "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist"))
  (declare (ignore config))
  (flet ((inject-p (x)
           (when (or force (katex-p x))
             (format nil *katex-header* location location location))))
    (add-injection #'inject-p :head)))
