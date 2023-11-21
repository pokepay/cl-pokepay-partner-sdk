(defpackage #:cl-pokepay-partner-sdk/crypto
  (:use #:cl)
  (:export #:aes256-encode-with-base64-url
           #:aes256-decode))
(in-package #:cl-pokepay-partner-sdk/crypto)

(defun make-aes256 (key)
  (ironclad:make-cipher :aes
                        :key key
                        :mode :cbc
                        :initialization-vector (ironclad:random-data 16)))

(defun aes256-encode (plaintext key)
  (let* ((aes (make-aes256 key))
         (len (length plaintext))
         (padded-len (+ 16 ; initialization vector length
                        len ; original plaintext length
                        (- 16 (mod len 16)))) ; padding length
         (padded-plaintext
           (let ((arr (make-array padded-len
                                  :element-type '(unsigned-byte 8)
                                  ;; filling by number of extra-bytes (pkcs#7)
                                  :initial-element (- padded-len (+ len 16)))))
             (replace arr plaintext :start1 16)))
         (ciphertext (make-array padded-len :element-type '(unsigned-byte 8))))
    (ironclad:encrypt aes padded-plaintext ciphertext)
    ciphertext))

(defun aes256-encode-with-base64-url (plaintext-string key)
  (jose/base64:base64url-encode (aes256-encode (babel:string-to-octets plaintext-string) key)))

(defun aes256-decode (ciphertext-base64-string key)
  (let* ((aes (make-aes256 key))
         (ciphertext (jose/base64:base64url-decode ciphertext-base64-string))
         (len (length ciphertext))
         (plaintext (make-array len :element-type '(unsigned-byte 8))))
    (ironclad:decrypt aes ciphertext plaintext)
    (let ((extra-bytes (aref plaintext (1- len))))
      (babel:octets-to-string plaintext :encoding :utf-8 :start 16 :end (- len extra-bytes)))))
