;; Copyright (C) 2014 Grim Schjetne

;; This file is part of CL-Arango.

;; CL-Arango is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; CL-Arango is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Foobar.  If not, see
;; <http://www.gnu.org/licenses/>.



(in-package :cl-arango)

;; Settings

(defvar *arango-host* "localhost")
(defvar *arango-port* 8529)


(defmacro with-arango-database ((host port) &rest body)
  `(let ((*arango-host* ,host)
         (*arango-port* ,port))
     ,@body))

;; Request apparatus

(defmacro def-arango-fun (name lambda-list method &rest args)
  `(progn
     (defun ,name ,lambda-list
       ,(cadr (assoc :documentation args))
       (send-request :method ,method
                     :uri (format-uri (list ,@(cdr (assoc :uri args)))
                                      ,@(cdr (assoc :query args)))
                     :content (with-output-to-string (stream)
                                ,@(cdr (assoc :content args)))))))

(defun format-uri (uri-params &optional query-params)
  (concatenate 'string "http://" *arango-host* ":"
                            (write-to-string *arango-port*) "/_api"
                            (apply #'concatenate 'string
                                   (append (mapcar (lambda (segment)
                                                     (concatenate 'string
                                                                  "/"
                                                                  segment))
                                                   uri-params)))
                            (if query-params
                                (apply #'concatenate 'string "?"
                                       (mapcar (lambda (pair)
                                                 (concatenate 'string
                                                              (car pair)
                                                              "="
                                                              (cond 
                                                                ((eq (cdr pair) t) "true") 
                                                                ((null (cdr pair)) "false")
                                                                (t (cdr pair)))
                                                              "&"))
                                               (plist-alist query-params))))))

(defun send-request (&key method uri content)
  (multiple-value-bind (stream status header)
      (http-request uri
                    :method method
                    :content content
                    :content-type "application/json; charset=utf-8"
                    :want-stream t)
    
    (let* ((content-type (cdr (assoc :content-type header)))
           (result (if (search "application/json" content-type)
                       (progn (setf (flexi-streams:flexi-stream-external-format stream)
                                    :utf-8)
                              (parse stream :object-as :alist)))))
      (if (and (>= status 200) (< status 300))
          result
          (error 'arangod-error
                 :code (cdr (assoc "code" result))
                 :num (cdr (assoc "errorNum" result))
                 :message (cdr (assoc "errorMessage" result)))))))

(define-condition arangod-error (error)
  ((code :accessor code :initarg :code)
   (num :accessor num :initarg :num)
   (message :accessor message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Code: ~D, number: ~D, message: ~A"
                     (code condition) (num condition) (message condition)))))

