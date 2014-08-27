(in-package #:arango)

(defun connect (&key (host *arango-host*) (port *arango-port*))
  (with-endpoint (host port)
    (list-accessible-databases)))

(defclass arango-object ()
  ((properties :initform (make-hash-table))))

(defclass database (arango-object)
  ((name :initarg :name)))

(defclass system-database (database))

(defclass document (arango-object)
  ((database :initform *system-database*)))

(defclass edge (arango-object))

(defclass collection (arango-object))

(defclass index (arango-object))

(defclass user (arango-object))

(defclass endpoint (arango-object)
  ((databases)))

(defgeneric delete-object (object)
  (:documentation "Removes the given OBJECT from the database"))

(defmethod delete-object ((db database))
  (drop-database (slot-value db 'name)
                 :database (slot-value *system-database* 'name)))

(defmethod delete-object ((doc document))
  (delete-document (slot-value doc 'handle)
                   :database (slot-value (slot-value doc 'database) 'name)))
