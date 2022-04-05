(defpackage :coleslaw-twitter-summary-card
  (:use :cl :coleslaw)
  (:export #:enable))

(in-package :coleslaw-twitter-summary-card)

(defun summary-card (post twitter-handle)
  (when (card-type-of post)
    (concatenate
     'string
     (format nil "<meta name=\"twitter:card\" content=\"~a\" />" (card-type-of post))
     (format nil "<meta name=\"twitter:author\" content=\"~a\" />" twitter-handle)
     (format nil "<meta property=\"og:url\" content=\"https:~a/~a\" />" (domain *config*) (page-url post))
     (format nil "<meta property=\"og:title\" content=\"~a\" />" (title-of post))
     (format nil "<meta property=\"og:description\" content=\"~a\" />" (description-of post))
     (when (image-of post)
       (format nil "<meta property=\"og:image\" content=\"~a~a\" />" (sitecdn *config*) (image-of post))))))

(defun enable (&key twitter-handle)
  (add-injection
   (lambda (x)
     (when (typep x 'post)
       (summary-card x twitter-handle)))
                 :head))
