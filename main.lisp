(defpackage #:cl-pokepay-partner-sdk/main
  (:nicknames #:cl-pokepay-partner-sdk)
  (:use #:cl
        #:cl-pokepay-partner-sdk/crypto)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export #:client
           #:request))
(in-package #:cl-pokepay-partner-sdk/main)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *allow-request-methods* '(:GET :POST :PATCH :UPDATE :DELETE)))

(deftype request-method ()
  `(member . ,*allow-request-methods*))

(defclass client ()
  ((base :initarg :base
         :initform (alexandria:required-argument :base)
         :reader client-base)
   (client-id :initarg :client-id
              :reader client-id)
   (client-secret :initarg :client-secret
                  :reader client-secret)
   (ssl-key-file :initarg :ssl-key-file
                 :reader client-ssl-key-file)
   (ssl-cert-file :initarg :ssl-cert-file
                  :reader client-ssl-cert-file)))

(defun now-timestring ()
  (local-time:format-timestring nil (local-time:now) :format local-time:+iso-8601-format+))

(defun random-uuid ()
  (string-downcase (princ-to-string (uuid:make-v4-uuid))))

(defun construct-content (client-id key request-data)
  (jzon:stringify
   (alexandria:alist-hash-table
    `(("partner_client_id" . ,client-id)
      ("data" . ,(aes256-encode-with-base64-url
                  (jzon:stringify
                   (alexandria:alist-hash-table
                    `(("request_data" . ,(jzon:parse request-data))
                      ("timestamp" . ,(now-timestring))
                      ("partner_call_id" . ,(random-uuid)))))
                  key))))))

(defmethod request ((client client) method path request-data)
  (check-type method request-method)
  (let* ((key (jose/base64:base64url-decode (client-secret client)))
         (content (construct-content (client-id client) key request-data))
         (ssl-key-file (client-ssl-key-file client))
         (ssl-cert-file (client-ssl-cert-file client)))
    (assert (uiop:file-exists-p ssl-key-file))
    (assert (uiop:file-exists-p ssl-cert-file))
    (handler-case
        (dex:request (quri:make-uri :path path :defaults (client-base client))
                     :method method
                     :headers `((:content-type . "application/json"))
                     :content content
                     :ssl-key-file (namestring (probe-file ssl-key-file))
                     :ssl-cert-file (namestring (probe-file ssl-cert-file)))
      (:no-error (response status headers &rest *)
        (assert (equal status 200))
        (assert (equal "application/json" (gethash "content-type" headers)))
        (multiple-value-bind (response-data response-data-exists)
            (gethash "response_data" (jzon:parse response))
          (assert response-data-exists)
          (jzon:parse (aes256-decode response-data key)))))))
