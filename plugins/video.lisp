(defpackage :coleslaw-video
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:index
                          #:index-content
                          #:content-text))

(in-package :coleslaw-video)

(defvar *video-header* "
<link href=\"~a/video-js.min.css\" rel=\"stylesheet\" />
<link href=\"~a/quality-selector.css\" rel=\"stylesheet\" />
")

(defvar *video-footer* "
<script src=\"~a/video.min.js\"></script>
<script src=\"~a/silvermine-videojs-quality-selector.min.js\"></script>
<script src=\"~a/video-quality.js\"></script>
")


(defun embedded-video-p (content)
  (not (null (search "<video" (content-text content)))))

(defgeneric video-p (document)
  (:documentation "Test if DOCUMENT requires contains any embedded videos.")
  (:method ((content content))
    (embedded-video-p content))
  (:method ((index index))
    (and (slot-boundp index 'content)
         (some #'video-p (index-content index)))))

(defun enable (&key force config (location "https://vjs.zencdn.net/7.8.4"))
  (declare (ignore config))
  (flet ((inject-head-p (x)
           (when (or force (video-p x))
             (format nil *video-header* location location)))
         (inject-body-p (x)
           (when (or force (video-p x))
             (format nil *video-footer* location location location))))
    (add-injection #'inject-head-p :head)
    (add-injection #'inject-body-p :body)))
