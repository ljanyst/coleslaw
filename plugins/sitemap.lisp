(defpackage :coleslaw-sitemap
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:index
                          #:page-url
                          #:mod-date-of
                          #:find-all
                          #:publish
                          #:theme-fn
                          #:add-document
                          #:write-document)
  (:import-from :alexandria #:hash-table-values)
  (:export #:enable))

(in-package :coleslaw-sitemap)

(defclass sitemap-element ()
  ((url :initarg :url :reader url)
   (moddate :initarg :moddate :reader moddate)))

(defclass sitemap (index)
  ((pages :initarg :pages :reader pages)))

(defmethod page-url ((object sitemap)) "sitemap.xml")

;; We do 'discovery' in the publish method here because we can't ensure the
;; sitemap discover method is called last. Need all other content to be
;; discovered/in the DB.
(defmethod publish ((doc-type (eql (find-class 'sitemap))))
  (let* ((pages (mapcar (lambda (page)
                          (make-instance 'sitemap-element :url (page-url page) :moddate (mod-date-of page)))
                        (hash-table-values coleslaw::*site*)))
         (sitemap (make-instance 'sitemap :pages pages)))
    (write-document sitemap (theme-fn 'sitemap "sitemap"))))

(defun enable ())
