(in-package #:arango)

(defvar *system-database* (make-instance 'database :name "_system"))

(defclass arango-object ()
  ((properties :initform (make-hash-table))))

(defclass database (arango-object)
  ((name :initarg :name)))

(defclass document (arango-object)
  ((database :initform *system-database*)))

(defclass edge (arango-object))

(defclass collection (arango-object))

(defclass index (arango-object))

(defclass user (arango-object))

(defclass endpoint (arango-object)
  ((databases)))

(defgeneric drop (object)
  (:documentation "Removes the given OBJECT from the database"))

(defmethod drop ((db database))
  (drop-database (slot-value db 'name)
                 :database (slot-value *system-database* 'name)))

(defmethod drop ((doc document))
  (delete-document (slot-value doc 'handle)
                   :database (slot-value (slot-value doc 'database) 'name)))
