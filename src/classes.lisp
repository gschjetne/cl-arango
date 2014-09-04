(in-package #:arango)

(defun connect (&key (host *arango-host*) (port *arango-port*))
  (with-endpoint (host port)
    (let ((database-names (jsown:val (list-accessible-databases) "result")))
      (apply #'values
             (mapcar (lambda (name)
                       (make-instance 'database :name name))
                     database-names)))))

(defclass database ()
  ((name :initarg :name)))

(defclass system-database (database))

(defclass document ()
  ((database :initform *system-database*)))

(defclass edge ())

(defclass collection ())

(defclass index ())

(defclass user ())

(defgeneric )

;; Deletion

(defgeneric delete-object (object)
  (:documentation "Removes the given OBJECT from the database"))

(defmethod delete-object ((db database))
  (drop-database (slot-value db 'name)
                 :database (slot-value *system-database* 'name)))

(defmethod delete-object ((doc document))
  (delete-document (slot-value doc 'handle)
                   :database (slot-value (slot-value doc 'database) 'name)))
