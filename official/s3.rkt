#lang racket/base
(require racket/list
         racket/file
         racket/path
         racket/system
         file/gzip
         racket/match
         "common.rkt"
         "notify.rkt")

(define (upload-all)
  (log! "upload: doing gzip")
  (gzip (format "~a/pkgs-all.json" static-path)
        (format "~a/pkgs-all.json.gz" static-path))

  (delete-file (format "~a/pkgs-all.json" static-path))

  (notify! "update upload in progress: there may be inconsistencies below")
  (log! "upload: uploading gzips")
  (system* s3cmd-path
           "-c" s3-config
           "sync"
           "-m" "application/javascript"
           "--acl-public"
           "--add-header" "Content-Encoding:gzip"
           "--delete-removed"
           (format "~a/pkgs-all.json.gz" static-path)
           (format "s3://~a/pkgs-all.json.gz" s3-bucket))

  (log! "upload: uploading everything else")
  (system* s3cmd-path
           "-c" s3-config
           "sync"
           "-M"
           "--acl-public"
           "--delete-removed"
           (format "~a/" static-path)
           (format "s3://~a/" s3-bucket))
  (log! "upload: done with upload")
  (notify! "")

  (void))

(define (upload-pkgs pkgs)
  ;; XXX make this more efficient
  (upload-all))
(define (run-s3! pkgs)
  (run! upload-pkgs pkgs))
(define run-sema (make-semaphore 1))
(define (signal-s3! pkgs)
  (safe-run! run-sema (λ () (run-s3! pkgs))))

(provide upload-pkgs
         signal-s3!)

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "s3"
   #:args pkgs
   (cond
     [(empty? pkgs)
      (upload-all)]
     [else
      (upload-pkgs pkgs)])))
