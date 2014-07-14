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
;; License along with Foobar.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package #:cl-arango)

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
  (:documentation "Retrieves the list of all existing databases.")
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
  (:documentation "Creates a new document in the collection named COLLECTION.")
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
   identified by COLLECTION.")
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
   identified by COLLECTION.")
  (:uri "edge")
  (:query `("collection" ,collection)))

(def-arango-fun create-edge (document collection from-handle to-handle
                                      &optional create-collection wait-for-sync)
  :post
  (:documentation "Creates a new edge document in the collection named COLLECTION")
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
(def-arango-fun read-edge-header (handle &optional rev)
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

;; AQL queries

;; Simple queries

(def-arango-fun simple-return-all (collection &optional skip limit)
  :put
  (:documentation "Returns all documents of a collection.")
  (:uri "simple" "all")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-by-example (example collection &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("example" . ,example)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-first-example (example collection)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "first-example")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("example" . ,example)) stream)))

(def-arango-fun simple-by-example-hash (example collection index
                                                &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example")
  (:content (encode-json-alist `(("index" . ,index)
                                 ("collection" . ,collection)
                                 ("example" . ,example)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-by-example-skiplist (example collection index
                                                    &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example-skiplist")
  (:content (encode-json-alist `(("index" . ,index)
                                 ("collection" . ,collection)
                                 ("example" . ,example)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-by-example-bitarray (example collection index
                                                    &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example-bitarray")
  (:content (encode-json-alist `(("index" . ,index)
                                 ("collection" . ,collection)
                                 ("example" . ,example)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-by-condition-bitarray (condition collection index
                                                        &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-condition-bitarray")
  (:content (encode-json-alist `(("index" . ,index)
                                 ("condition" . ,condition)
                                 ("collection" . ,collection)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-any (collection)
  :put
  (:documentation "Returns a random document from a collection.")
  (:uri "simple" "any")
  (:content (encode-json-plist `("collection" ,collection) stream)))

(def-arango-fun simple-range (collection attribute left right
                                         &optional closed skip limit)
  :put
  (:documentation
   "This will find all documents within a given range. In order to
   execute a range query, a skip-list index on the queried attribute
   must be present.")
  (:uri "simple" "range")
  (:content (encode-json-alist `(("attribute" . ,attribute)
                                 ("left" . ,left)
                                 ("right" . ,right)
                                 ("closed" . ,(t-or-f closed))
                                 ("collection" . ,collection)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-near (collection latitude longitude
                                        &optional distance geo skip limit)
  :put
  (:documentation
   "The default will find at most 100 documents near the given
   coordinate. The returned list is sorted according to the distance,
   with the nearest document being first in the list.")
  (:uri "simple" "near")
  (:content (encode-json-alist `(("latitude" . ,latitude)
                                 ("longitude" . ,longitude)
                                 ("collection" . ,collection)
                                 ,(if distance `("distance" . ,distance))
                                 ,(if geo `("geo" . ,geo))
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-within (collection latitude longitude radius
                                          &optional distance geo skip limit)
  :put
  (:documentation
   "This will find all documents within a given radius around the
  coordinate (LATITUDE, LONGITUDE). The returned list is sorted by
  distance.")
  (:uri "simple" "within")
  (:content (encode-json-alist `(("latitude" . ,latitude)
                                 ("longitude" . ,longitude)
                                 ("collection" . ,collection)
                                 ("radius" . ,radius)
                                 ,(if distance `("distance" . ,distance))
                                 ,(if geo `("geo" . ,geo))
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-fulltext (collection attribute query index
                                            &optional skip limit)
  :put
  (:documentation
   "This will find all documents from the collection that match the
   fulltext query specified in QUERY.")
  (:uri "simple" "fulltext")
  (:content (encode-json-alist `(("attribute" . ,attribute)
                                 ("query" . ,query)
                                 ("index" . ,index)
                                 ("collection" . ,collection)
                                 ,(if skip `("skip" . ,skip))
                                 ,(if limit `("limit" . ,limit)))
                               stream)))

(def-arango-fun simple-remove-by-example (example collection
                                                  &optional wait-for-sync limit)
  :put
  (:documentation
   "Finds and removes all documents matching the example given by EXAMPLE.")
  (:uri "simple" "remove-by-example")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("example" . ,example)
                                 ("options" . ,(list (if wait-for-sync
                                                         `("waitForSync" . ,wait-for-sync))
                                                     (if limit
                                                         `("limit" . ,limit)))))
                               stream)))

(def-arango-fun simple-replace-by-example (example collection replacement
                                                  &optional wait-for-sync limit)
  :put
  (:documentation
   "Finds and replaces all documents matching EXAMPLE by REPLACEMENT.")
  (:uri "simple" "replace-by-example")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("example" . ,example)
                                 ("replacement" . ,replacement)
                                 ("options" . ,(list (if wait-for-sync
                                                         `("waitForSync" . ,wait-for-sync))
                                                     (if limit
                                                         `("limit" . ,limit)))))
                               stream)))

(def-arango-fun simple-update-by-example (example collection replacement
                                                  &optional keep-null wait-for-sync limit)
  :put
  (:documentation
   "Finds all documents matching EXAMPLE and partially updates them
   with REPLACEMENT.")
  (:uri "simple" "update-by-example")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("example" . ,example)
                                 ("replacement" . ,replacement)
                                 ("options" . ,(list (if keep-null
                                                         `("keepNull" . ,keep-null))
                                                     (if wait-for-sync
                                                         `("waitForSync" . ,wait-for-sync))
                                                     (if limit
                                                         `("limit" . ,limit)))))
                               stream)))

(def-arango-fun simple-first (collection &optional (count 1))
  :put
  (:documentation
   "Returns the first document(s) from COLLECTION, in the order of
   insertion/update time. When the COUNT argument is supplied, the
   result will be a list of documents, with the oldest document being
   first in the result list")
  (:uri "simple" "first")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("count" . ,count)) stream)))

(def-arango-fun simple-last (collection &optional (count 1))
  :put
  (:documentation
   "Returns the last document(s) from COLLECTION, in the order of
   insertion/update time. When the COUNT argument is supplied, the
   result will be a list of documents, with the latest document being
   first in the result list")
  (:uri "simple" "last")
  (:content (encode-json-alist `(("collection" . ,collection)
                                 ("count" . ,count)) stream)))
