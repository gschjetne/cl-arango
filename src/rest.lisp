;;; Copyright © 2014 Grim Schjetne <gs@schjetne.se>
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



(in-package #:cl-arango)

(defun t-or-f (x)
  (if x "true" "false"))

;; Databases

(def-arango-fun get-current-database ()
  :get
  (:documentation "Retrieves information about the current database.")
  (:uri "database" "current"))

(def-arango-fun list-accessible-databases ()
  :get
  (:documentation
   "Retrieves the list of all databases the current user can access
  without specifying a different username or password.")
  (:uri "database" "user"))

(def-arango-fun list-databases ()
  :get
  (:documentation "Retrieves the list of all existing databases".)
  (:uri "database"))

(def-arango-fun create-database (name &optional users)
  :post
  (:documentation "Creates a new database.")
  (:uri "database")
  (:content (encode-json-alist (list (cons "name" name)
                                     (if users
                                         (cons "users" users))) stream)))

(def-arango-fun drop-database (name)
  :delete
  (:documentation "Deletes the database along with all data stored in it.")
  (:uri "database" name))

;; Documents

(def-arango-fun read-document (handle)
  :get
  (:documentation
   "Returns the document identified by HANDLE.")
  (:uri "document" handle))

(def-arango-fun create-document (document collection &optional
                                          create-collection wait-for-sync)
  :post
  (:documentation "Creates a new document in the collection named collection.")
  (:uri "document")
  (:query (list "collection" collection
                "createCollection" create-collection
                "waitForSync" wait-for-sync))
  (:content (encode-json-alist document stream)))

(def-arango-fun replace-document (document handle &optional
                                           wait-for-sync rev policy)
  :put
  (:documentation
   "Completely updates (i.e. replaces) the document identified by HANDLE.")
  (:uri "document" handle)
  (:query (append `("waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content (encode-json-alist document stream)))

(def-arango-fun patch-document (document handle &optional
                                         keep-null wait-for-sync rev policy)
  :patch
  (:documentation "Partially updates the document identified by HANDLE.")
  (:uri "document" handle)
  (:query (append `("keepNull" ,keep-null "waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content (encode-json-alist document stream)))

(def-arango-fun delete-document (handle &optional wait-for-sync rev policy)
  :delete
  (:documentation "Deletes a document.")
  (:uri "document" handle)
  (:query (append `("waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy)))))

;; This function always fails. Custom response handler is needed
(def-arango-fun read-document-header (handle &optional rev)
  :head
  (:documentation
   "Like READ-DOCUMENT, but only returns the header fields and not the body.")
  (:uri "document" handle)
  (:query (if rev `("rev" rev))))

(def-arango-fun read-all-documents (collection)
  :get
  (:documentation
   "Returns a list of all URI for all documents from the collection
   identified by collection.")
  (:uri "document")
  (:query `("collection" ,collection)))

;; Edges

(def-arango-fun read-edge (handle)
  :get
  (:documentation "Returns the edge identified by HANDLE.")
  (:uri "edge" handle))

(def-arango-fun read-all-edges (collection)
  :get
  (:documentation
   "Returns a list of all URI for all edges from the collection
   identified by collection.")
  (:uri "edge")
  (:query `("collection" ,collection)))

(def-arango-fun create-edge (document collection from-handle to-handle
                                      &optional create-collection wait-for-sync)
  :post
  (:documentation "Creates a new edge document in the collection named collection")
  (:uri "edge")
  (:query (list "collection" collection
                "from" from-handle
                "to" to-handle
                "createCollection" create-collection
                "waitForSync" wait-for-sync))
  (:content (encode-json-alist document stream)))

(def-arango-fun patch-edge (document handle &optional
                                     keep-null wait-for-sync rev policy)
  :patch
  (:documentation "Partially updates the edge document identified by HANDLE.")
  (:uri "edge" handle)
  (:query (append `("keepNull" ,keep-null "waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content (encode-json-alist document stream)))

(def-arango-fun replace-edge (document handle &optional
                                       wait-for-sync rev policy)
  :put
  (:documentation
   "Completely updates (i.e. replaces) the edge identified by HANDLE.")
  (:uri "edge" handle)
  (:query (append `("waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content (encode-json-alist document stream)))

(def-arango-fun delete-edge (handle &optional wait-for-sync rev policy)
  :delete
  (:documentation "Deletes an edge.")
  (:uri "edge" handle)
  (:query (append `("waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy)))))

;; This function always fails. Custom response handler is needed
(def-arango-fun read-document-header (handle &optional rev)
  :head
  (:documentation
   "Like READ-EDGE, but only returns the header fields and not the body.")
  (:uri "document" handle)
  (:query (if rev `("rev" rev))))

(def-arango-fun read-in-or-outbound-edges (collection vertex &optional direction)
  :get
  (:documentation
   "Returns the list of edges starting or ending in the vertex
   identified by VERTEX.")
  (:uri "edges" collection)
  (:query (append `("vertex" ,vertex)
                  (if direction
                      `("direction" ,direction)))))

