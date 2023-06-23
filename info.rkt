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
               ["https://github.com/racket/infrastructure-userdb.git#main" #:version "0.1"]
               "s3-sync"
               "plt-service-monitor"))
(define build-deps '("rackunit-lib"))
(define license
  '(Apache-2.0 OR MIT))
