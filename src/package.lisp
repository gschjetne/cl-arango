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

(in-package #:cl-user)

(defpackage #:cl-arango-rest
  (:use #:cl
        #:alexandria
        #:anaphora
	#:drakma)
  
  (:export #:get-current-database
           #:list-accessible-databases
           #:list-databases
           #:create-database
           #:drop-database
           
           #:read-document
           #:create-document
           #:replace-document
           #:patch-document
           #:delete-document
           #:read-document-header
           #:read-all-documents

           #:read-edge
           #:read-all-edges
           #:create-edge
           #:patch-edge
           #:replace-edge
           #:delete-edge
           #:read-document-header
           #:read-in-or-outbound-edges

           #:aql-query
           #:create-cursor
           #:read-cursor
           #:delete-cursor
           #:parse-query
           #:explain-query
           #:create-user-fun
           #:delete-user-fun
           #:list-user-funs

           #:simple-return-all
           #:simple-by-example
           #:simple-first-example
           #:simple-by-example-hash
           #:simple-by-example-skiplist
           #:simple-by-example-bitarray
           #:simple-by-condition-bitarray
           #:simple-any
           #:simple-range
           #:simple-near
           #:simple-within
           #:simple-fulltext
           #:simple-remove-by-example
           #:simple-replace-by-example
           #:simple-update-by-example
           #:simple-first
           #:simple-last

           #:key-options
           #:create-collection
           #:delete-collection
           #:truncate-collection
           #:read-collection-properties
           #:read-collection-document-count
           #:read-collection-statistics
           #:read-collection-revision-id
           #:read-collection-checksum
           #:read-all-collections
           #:load-collection
           #:unload-collection
           #:set-collection-properties
           #:rename-collection
           #:collection-rotate-journal

           #:read-index
           #:create-index
           #:cap-constraint
           #:hash-index
           #:skip-list
           #:geo
           #:full-text
           #:delete-index
           #:read-all-indexes

           #:create-graph

           #:traverse

           #:database-user
           #:create-user
           #:replace-user
           #:update-user
           #:delete-user
           #:read-user

           #:create-endpoint
           #:delete-endpoint
           #:list-endpoints

           #:with-endpoint
           #:with-database
           #:with-user

           #:t-or-jsf
           #:t-or-jsf-p

           #:arango-error
           #:http-status
           #:error-number
           #:error-message

           #:*arango-host*
           #:*arango-port*
           #:*arango-database*
           #:*parse-result*
           #:*username*
           #:*password*))
