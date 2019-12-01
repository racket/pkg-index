#lang info

(define name "Package server")
(define compile-omit-paths 'all)
(define test-omit-paths 'all)
(define test-responsibles '((all jay)))
(define deps '("racket-lib"
               "base"
               "compatibility-lib"
               "net-lib"
               "web-server-lib"
               "bcrypt"
               "s3-sync"
               "plt-service-monitor"))
(define build-deps '("rackunit-lib"))
