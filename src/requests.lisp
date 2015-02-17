;; Copyright © 2014 Grim Schjetne <gs@schjetne.se>

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
;; License along with CL-Arango.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package #:cl-arango-rest)

;; Settings

(defvar *arango-host* "localhost")
(defvar *arango-port* 8529)
(defvar *arango-database* "_system")
(defvar *username* nil)
(defvar *password* nil)
(defvar *parse-result* t)

(defmacro with-endpoint ((host port) &body body)
  "Select the host and port to connect to"
  `(let ((*arango-host* ,host)
         (*arango-port* ,port))
     ,@body))

(defmacro with-database (name &body body)
  "Select the database NAME in which to do the operations in"
  `(let ((*arango-database* ,name))
     ,@body))

(defmacro with-user ((username password) &body body)
  "Authenticate with USERNAME and PASSWORD"
  `(let ((*username* ,username)
         (*password* ,password))
     ,@body))

;; Request apparatus

(defmacro def-arango-fun (name lambda-list method &rest args)
  `(defun ,name ,lambda-list
     ,(cadr (assoc :documentation args))
     (send-request :method ,method
                   :uri (format-uri ,@(remove nil (append (assoc :uri args)
                                                          (assoc :query args)))
                                    :database *arango-database*)
                   :username *username*
                   :password *password*
                   :content (aif ,(cadr (assoc :content args))
                                 (jsown:to-json it)))))

(defun format-uri (&key uri query database)
  (concatenate 'string "http://" *arango-host* ":"
               (write-to-string *arango-port*)
               (if database
                   (concatenate 'string
                                "/_db/"
                                database))
               "/_api"
               (apply #'concatenate 'string
                      (append (mapcar (lambda (segment)
                                        (concatenate 'string
                                                     "/"
                                                     segment))
                                      uri)))
               (if query
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
                                  (plist-alist query))))))

(defun send-request (&key method uri content username password)
  (multiple-value-bind (body status header)
      (http-request uri
                    :method method
                    :content content
                    :external-format-out :utf-8
                    :content-type "application/json; charset=utf-8"
                    :basic-authorization (if (and username password)
                                             (list username password)))
    
    (let* ((content-type (cdr (assoc :content-type header)))
           (body-string (flexi-streams:octets-to-string body
                                                        :external-format :utf-8))
           (result (if (search "application/json" content-type)
                       (jsown:parse body-string))))
      (if (and (jsown:keyp result "error") (t-or-jsf-p (jsown:val result "error")))
          (restart-case
              (error 'arango-error
                     :http-status status
                     :error-number (jsown:val result "errorNum")
                     :error-message (jsown:val result "errorMessage"))
            (return-result () (if *parse-result* result body-string)))
          (if *parse-result* result body-string)))))

;; Conditions

(define-condition arango-error (error)
  ((http-status :initarg :http-status :reader http-status)
   (error-number :initarg :error-number :reader error-number)
   (error-message :initarg :error-message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "ArangoDB failed with status ~D/~D: ~A"
                     (http-status condition)
                     (error-number condition)
                     (error-message condition)))))

;; Utility functions

(defun t-or-jsf (x)
  (if x t :f))

(defun t-or-jsf-p (x)
  (when (not (or (eq x :f)
                 (eq x :false))) x))
