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

(def-arango-fun create-database (name &rest users)
  :post
  (:documentation "Creates a new database.")
  (:uri "database")
  (:content (cons :obj
                  (remove nil `(("name" . ,name)
                                ,(if users `("users" . ,users)))))))

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
  (:content document))

(def-arango-fun replace-document (document handle &optional
                                           wait-for-sync rev policy)
  :put
  (:documentation
   "Completely updates (i.e. replaces) the document identified by HANDLE.")
  (:uri "document" handle)
  (:query (append `("waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content document))

(def-arango-fun patch-document (document handle &optional
                                         keep-null wait-for-sync rev policy)
  :patch
  (:documentation "Partially updates the document identified by HANDLE.")
  (:uri "document" handle)
  (:query (append `("keepNull" ,keep-null "waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content document))

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
  (:content document))

(def-arango-fun patch-edge (document handle &optional
                                     keep-null wait-for-sync rev policy)
  :patch
  (:documentation "Partially updates the edge document identified by HANDLE.")
  (:uri "edge" handle)
  (:query (append `("keepNull" ,keep-null "waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content document))

(def-arango-fun replace-edge (document handle &optional
                                       wait-for-sync rev policy)
  :put
  (:documentation
   "Completely updates (i.e. replaces) the edge identified by HANDLE.")
  (:uri "edge" handle)
  (:query (append `("waitForSync" ,wait-for-sync)
                  (if rev `("rev" ,rev))
                  (if policy `("policy" ,policy))))
  (:content document))

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
  (:content (cons :obj (remove nil `(("collection" . ,collection)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-by-example (example collection &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example")
  (:content (cons :obj (remove nil `(("collection" . ,collection)
                                     ("example" . ,example)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-first-example (example collection)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "first-example")
  (:content `(:obj ("collection" . ,collection)
                   ("example" . ,example))))

(def-arango-fun simple-by-example-hash (example collection index
                                                &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example")
  (:content (cons :obj (remove nil `(("index" . ,index)
                                     ("collection" . ,collection)
                                     ("example" . ,example)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-by-example-skiplist (example collection index
                                                    &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example-skiplist")
  (:content (cons :obj (remove nil `(("index" . ,index)
                                     ("collection" . ,collection)
                                     ("example" . ,example)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-by-example-bitarray (example collection index
                                                    &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-example-bitarray")
  (:content (cons :obj (remove nil `(("index" . ,index)
                                     ("collection" . ,collection)
                                     ("example" . ,example)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-by-condition-bitarray (condition collection index
                                                        &optional skip limit)
  :put
  (:documentation "Finds all documents matching the example given by EXAMPLE.")
  (:uri "simple" "by-condition-bitarray")
  (:content (cons :obj (remove nil `(("index" . ,index)
                                     ("condition" . ,condition)
                                     ("collection" . ,collection)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-any (collection)
  :put
  (:documentation "Returns a random document from a collection.")
  (:uri "simple" "any")
  (:content `(:obj ("collection" . ,collection))))

(def-arango-fun simple-range (collection attribute left right
                                         &optional closed skip limit)
  :put
  (:documentation
   "This will find all documents within a given range. In order to
   execute a range query, a skip-list index on the queried attribute
   must be present.")
  (:uri "simple" "range")
  (:content (cons :obj (remove nil `(("attribute" . ,attribute)
                                     ("left" . ,left)
                                     ("right" . ,right)
                                     ("closed" . ,(t-or-f closed))
                                     ("collection" . ,collection)
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-near (collection latitude longitude
                                        &optional distance geo skip limit)
  :put
  (:documentation
   "The default will find at most 100 documents near the given
   coordinate. The returned list is sorted according to the distance,
   with the nearest document being first in the list.")
  (:uri "simple" "near")
  (:content (cons :obj (remove nil `(("latitude" . ,latitude)
                                     ("longitude" . ,longitude)
                                     ("collection" . ,collection)
                                     ,(if distance `("distance" . ,distance))
                                     ,(if geo `("geo" . ,geo))
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-within (collection latitude longitude radius
                                          &optional distance geo skip limit)
  :put
  (:documentation
   "This will find all documents within a given radius around the
  coordinate (LATITUDE, LONGITUDE). The returned list is sorted by
  distance.")
  (:uri "simple" "within")
  (:content (cons :obj (remove nil `(("latitude" . ,latitude)
                                     ("longitude" . ,longitude)
                                     ("collection" . ,collection)
                                     ("radius" . ,radius)
                                     ,(if distance `("distance" . ,distance))
                                     ,(if geo `("geo" . ,geo))
                                     ,(if skip `("skip" . ,skip))
                                     ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-fulltext (collection attribute query index
                                            &optional skip limit)
  :put
  (:documentation
   "This will find all documents from the collection that match the
   fulltext query specified in QUERY.")
  (:uri "simple" "fulltext")
  (:content (cons :obj (remove nil `(("attribute" . ,attribute)
                                        ("query" . ,query)
                                        ("index" . ,index)
                                        ("collection" . ,collection)
                                        ,(if skip `("skip" . ,skip))
                                        ,(if limit `("limit" . ,limit)))))))

(def-arango-fun simple-remove-by-example (example collection
                                                  &optional wait-for-sync limit)
  :put
  (:documentation
   "Finds and removes all documents matching the example given by EXAMPLE.")
  (:uri "simple" "remove-by-example")
  (:content `(:obj ("collection" . ,collection)
                   ("example" . ,example)
                   ("options" . ,(remove nil
                                         (list
                                          :obj
                                          (if wait-for-sync
                                              `("waitForSync" . ,wait-for-sync))
                                          (if limit
                                              `("limit" . ,limit))))))))

(def-arango-fun simple-replace-by-example (example collection replacement
                                                  &optional wait-for-sync limit)
  :put
  (:documentation
   "Finds and replaces all documents matching EXAMPLE by REPLACEMENT.")
  (:uri "simple" "replace-by-example")
  (:content (cons :obj `(("collection" . ,collection)
                         ("example" . ,example)
                         ("replacement" . ,replacement)
                         ("options" . ,(remove nil
                                               (list
                                                :obj
                                                (if wait-for-sync
                                                    `("waitForSync" . ,wait-for-sync))
                                                (if limit
                                                    `("limit" . ,limit)))))))))

(def-arango-fun simple-update-by-example (example collection replacement
                                                  &optional keep-null wait-for-sync limit)
  :put
  (:documentation
   "Finds all documents matching EXAMPLE and partially updates them
   with REPLACEMENT.")
  (:uri "simple" "update-by-example")
  (:content `(:obj ("collection" . ,collection)
                   ("example" . ,example)
                   ("replacement" . ,replacement)
                   ("options" . ,(remove nil
                                         (list
                                          :obj
                                          (if keep-null
                                              `("keepNull" . ,keep-null))
                                          (if wait-for-sync
                                              `("waitForSync" . ,wait-for-sync))
                                          (if limit
                                              `("limit" . ,limit))))))))

(def-arango-fun simple-first (collection &optional (count 1))
  :put
  (:documentation
   "Returns the first document(s) from COLLECTION, in the order of
   insertion/update time. When the COUNT argument is supplied, the
   result will be a list of documents, with the oldest document being
   first in the result list")
  (:uri "simple" "first")
  (:content `(:obj ("collection" . ,collection)
                   ("count" . ,count))))

(def-arango-fun simple-last (collection &optional (count 1))
  :put
  (:documentation
   "Returns the last document(s) from COLLECTION, in the order of
   insertion/update time. When the COUNT argument is supplied, the
   result will be a list of documents, with the latest document being
   first in the result list")
  (:uri "simple" "last")
  (:content `(:obj ("collection" . ,collection)
                   ("count" . ,count))))


;; Collections

(defun key-options (type allow-user-keys &optional increment offset)
  "Constructs a hash table of key options suitable for passing to CREATE-COLLECTION"
  (cons :obj (remove nil
                     (append
                      (ccase type
                        (:traditional '(("type" . "traditional")))
                        (:autoincrement `(("type" . "autoincrement")
                                          ,(if increment
                                               `("increment" . ,increment))
                                          ,(if offset
                                               `("offset" . ,offset)))))
                      `(("allowUserKeys" . ,(t-or-f allow-user-keys)))))))

(def-arango-fun create-collection (name &optional wait-for-sync do-compact
                                        journal-size is-system is-volatile
                                        key-options (type :document) number-of-shards
                                        (shard-keys '("_key")))
  :post
  (:documentation "Creates an new collection with a given name.")
  (:uri "collection")
  (:content (cons :obj
                  (remove nil `(("name" . ,name)
                                ("type" . ,(ccase type
                                                  (:document 2)
                                                  (:edges 3)))
                                ,(if wait-for-sync
                                     `("waitForSync" . ,wait-for-sync))
                                ,(if do-compact
                                     `("doCompact" . ,do-compact))
                                ,(if journal-size
                                     `("journalSize" . ,journal-size))
                                ,(if is-system
                                     `("isSystem" . ,is-system))
                                ,(if is-volatile
                                     `("isVolatile" . ,is-volatile))
                                ,(if key-options
                                     `("keyOptions" . ,key-options))
                                ,(if number-of-shards
                                     `("numberOfShards" . ,number-of-shards))
                                ,(if number-of-shards
                                     `("shardKeys" . ,shard-keys)))))))

(def-arango-fun delete-collection (name)
  :delete
  (:documentation "Deletes a collection identified by NAME.")
  (:uri "collection" name))

(def-arango-fun truncate-collection (name)
  :put
  (:documentation
   "Removes all documents from the collection, but leaves the indexes intact.")
  (:uri "collection" name "truncate"))

(def-arango-fun read-collection-properties (name)
  :get
  (:documentation "Read properties of a collection identified by NAME.")
  (:uri "collection" name "properties"))

(def-arango-fun read-collection-document-count (name)
  :get
  (:documentation
   "Return number of documents in a collection identified by NAME.")
  (:uri "collection" name "count"))

(def-arango-fun read-collection-statistics (name)
  :get
  (:documentation "Return statistics for a collection identified by NAME.")
  (:uri "collection" name "figures"))

(def-arango-fun read-collection-revision-id (name)
  :get
  (:documentation "Return the revision ID of a collection identified by NAME.")
  (:uri "collection" name "revision"))

(def-arango-fun read-collection-checksum (name)
  :get
  (:documentation "Return the checksum of a collection identified by NAME.")
  (:uri "collection" name "checksum"))

(def-arango-fun read-all-collections ()
  :get
  (:documentation "Return all collections")
  (:uri "collection"))

(def-arango-fun load-collection (name &optional count)
  :put
  (:documentation
   "Loads a collection into memory. If COUNT is set to T, number of
  documents will also be returned, at a performance penalty")
  (:uri "collection" name "load")
  (:query (if count `("count" ,count))))

(def-arango-fun unload-collection (name)
  :put
  (:documentation
   "Removes a collection from memory. This call does not delete any documents.")
  (:uri "collection" name "unload"))

(def-arango-fun set-collection-properties (name wait-for-sync journal-size)
  :put
  (:documentation "Changes the properties of a collection.")
  (:uri "collection" name "properties")
  (:content `(:obj ("waitForSync" . ,(t-or-jsf wait-for-sync))
                   ("journalSize" . ,journal-size))))

(def-arango-fun rename-collection (name new-name)
  :put
  (:document "Renames a collection identified by NAME to NEW-NAME.")
  (:uri "collection" name "rename")
  (:content `(:obj ("name" . ,new-name))))

(def-arango-fun collection-rotate-journal (name)
  :put
  (:document
   "Rotates the journal of a collection. The current journal of the
   collection will be closed and made a read-only datafile. The
   purpose of the rotate method is to make the data in the file
   available for compaction (compaction is only performed for
   read-only datafiles, and not for journals).")
  (:uri "collection" name "rotate"))


;; Indexes

(def-arango-fun read-index (handle)
  :get
  (:documentation "")
  (:uri "index" handle))

(def-arango-fun create-index (collection details)
  :post
  (:documentation "Creates a new index in the collection COLLECTION.")
  (:uri "index")
  (:query `("collection" ,collection))
  (:content details))

(defun cap-constraint (&optional size byte-size)
  `(:obj ("type" . "cap")
         ("size" . ,size)
         ("byteSize" . ,byte-size)))

(defun hash-index (fields &optional unique)
  `(:obj ("type" . "hash")
         ("fields" . ,fields)
         ("unique" . ,(t-or-jsf unique))))

(defun skip-list (fields &optional unique)
  `(:obj ("type" . "skiplist")
         ("fields" . ,fields)
         ("unique" . ,(t-or-jsf unique))))

(defun geo (fields)
  `(:obj ("type" . "geo")
         ("fields" ,fields)))

(defun full-text (fields min-length)
  `(:obj ("type" . "fulltext")
         ("fields" . ,fields)
         ("minLength" . ,min-length)))

(def-arango-fun delete-index (handle)
  :delete
  (:documentation "Deletes an index identified by HANDLE.")
  (:uri "index" handle))

(def-arango-fun read-all-indexes (collection)
  :get
  (:documentation
   "Returns an object with an attribute indexes containing a list of
   all index descriptions for the given collection.")
  (:uri "index")
  (:query `("collection" ,collection)))


;; Transactions

;; General Graph

;; Traversals

;; Replication

;; Bulk Imports

;; Batch Requests

;; Monitoring


;; User Management

(defun database-user (username &optional password (active t) extra)
  "Constructs a list defining a user, suitable for passing to
CREATE-DATABASE and CREATE-USER."
  (cons :obj (remove nil `(("username" . ,username)
                           ("active" . ,(t-or-jsf active))
                           ,(if password `("passwd" . ,password))
                           ,(if extra `("extra" . ,extra))))))

(def-arango-fun create-user (database-user &optional change-password)
  :post
  (:documentation "")
  (:uri "user")
  (:content (append database-user
                    `(("changePassword" . ,(t-or-jsf change-password))))))

(defmacro replace-or-update-user (name method documentation)
  `(def-arango-fun ,name (username password &optional
                                   (active t) extra change-password)
     ,method
     (:documentation ,documentation)
     (:uri "user" username)
     (:content (cons :obj (remove nil `(("passwd" . ,password)
                                        ("active" . ,(t-or-jsf active))
                                        ,(if password
                                             `("changePassword" . ,(t-or-jsf change-password)))
                                        ,(if extra `("extra" . ,extra))))))))

(replace-or-update-user replace-user :put
                        "Replaces the data of an existing user.")
(replace-or-update-user update-user :patch
                        "Updates the data of an existing user.")

(def-arango-fun delete-user (username)
  :delete
  (:documentation "Removes an existing user, identified by USER.")
  (:uri "user" username))

(def-arango-fun read-user (username)
  :get
  (:documentation "Fetches data about the specified user.")
  (:uri "user" username))

;; Async Result

;; Endpoints

(def-arango-fun create-endpoint (endpoint &rest databases)
  :post
  (:documentation
   "Add a new endpoint, or reconfigure an existing one. If DATABASES
   is NIL, all databases present in the server will become accessible
   via the endpoint, with the _system database being the default
   database.")
  (:uri "endpoint")
  (:content `(:obj ("endpoint" . ,endpoint)
                   ("databases" . ,databases))))

(def-arango-fun delete-endpoint (endpoint)
  :delete
  (:documentation
   "This operation deletes an existing endpoint from the list of all
   endpoints, and makes the server stop listening on the endpoint.")
  (:uri "endpoint" endpoint))

(def-arango-fun list-endpoints ()
  :get
  (:documentation
   "Returns a list of all configured endpoints the server is listening
  on. For each endpoint, the list of allowed databases is returned too
  if set.")
  (:uri "endpoint"))


;; Sharding

;; Miscellaneous

;; General Handling
