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

(defpackage :cl-arango-system (:use :cl :asdf))
(in-package :cl-arango-system)

(defsystem cl-arango
  :name "CL-Arango"
  :author "Grim Schjetne <gs@schjetne.se"
  :version "unreleased"
  :description "ArangoDB client library"
  :long-description "ArangoDB REST client library for Common Lisp"
  :depends-on (:alexandria
               :anaphora
               :drakma
               :jsown)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "requests")
             (:file "rest" :depends-on ("requests"))))))
