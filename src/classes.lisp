(in-package #:arango)

(defclass server ()
  ((host :initarg :host :initform *arango-host*)
   (port :initarg :port :initform *arango-port*)
   (username :initarg :username :initform *username*)
   (password :initarg :password :initform *password*)))

(defmacro with-server (server &body body)
  `(with-endpoint ((slot-value ,server 'host)
                   (slot-value ,server 'port))
     (with-user ((slot-value ,server 'username)
                 (slot-value ,server 'password))
       ,@body)))

(defgeneric get-database-names (server))

(defmethod get-database-names ((server server))
  (with-server server
    (jsown:val (list-accessible-databases) "result")))

(defgeneric get-databases (server)
  (:documentation "Return databases served by SERVER"))

(defmethod get-databases ((server server))
  (mapcar (lambda (name)
            (make-instance 'database
                           :name name
                           :server server))
          (get-database-names server)))

(defgeneric get-database (server name)
  (:documentation "Gets a new database from"))

(defmethod get-database ((server server) name)
  (if (member name (get-database-names server) :test #'equal)
      (make-instance 'database
                     :name name
                     :server server)))

(defgeneric new-database (server name))

(defmethod new-database ((server server) name)
  (with-server server
    (create-database name)
    (get-database server name)))

(defclass database ()
  ((name :initarg :name)
   (server :initarg :server)))

(defgeneric get-server (object))

(defmethod get-server ((object database))
  (slot-value object 'server))

(defclass user ()
  ((name :initarg :name)))



;; Deletion

(defgeneric delete-object (object)
  (:documentation "Removes the given OBJECT from the database"))

(defmethod delete-object ((object database))
  (with-server (get-server object)
    (drop-database (slot-value object 'name))))
