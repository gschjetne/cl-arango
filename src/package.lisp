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



(in-package #:cl-user)

(defpackage #:cl-arango
  (:use #:cl
        #:alexandria
        #:anaphora
        #:yason
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
           
	   ;; Vars
	   #:*arango-host*
	   #:*arango-port*))
