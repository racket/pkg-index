#lang info

(define collection "pkg-index")
(define name "Package server")

(define test-responsibles '((all jay)))
(define deps '("bcrypt"
               "compatibility-lib"
               "net-lib"
               "base"
               "plt-services"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
