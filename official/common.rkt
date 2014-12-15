#lang racket/base
(require racket/file
         racket/runtime-path
         pkg/util
         racket/match
         racket/list
         racket/date
         racket/system
         racket/string
         web-server/http/id-cookie)

(define-runtime-path src ".")

(define-runtime-path root "root")
(make-directory* root)
(define secret-key
  (make-secret-salt/file
   (build-path root "secret.key")))
(define users-path (build-path root "users"))
(make-directory* users-path)
(define users.new-path (build-path root "users.new"))
(make-directory* users.new-path)

(github-client_id (file->string (build-path root "client_id")))
(github-client_secret (file->string (build-path root "client_secret")))

(define cache-path (build-path root "cache"))
(make-directory* cache-path)

(define SUMMARY-NAME "summary.rktd")
(define SUMMARY-PATH (build-path cache-path SUMMARY-NAME))

(define pkgs-path (build-path root "pkgs"))
(make-directory* pkgs-path)

(define static.src-path (build-path src "static"))
(define static-path (build-path src "static-gen"))
(define notice-path (format "~a/notice.json" static-path))

(define (package-list)
  (sort (map path->string (directory-list pkgs-path))
        string-ci<=?))
(define (package-info pkg-name #:version [version #f])
  (define p 
    (build-path pkgs-path pkg-name))
  (define v
    (if (file-exists? p)
        (file->value p)
        (hasheq)))
  (define ht
    (if (hash? v)
        v
        (hasheq)))
  (define no-version 
    (hash-set ht 'name pkg-name))
  (cond
    [(and version
          (hash-has-key? no-version 'versions)
          (hash? (hash-ref no-version 'versions #f))
          (hash-has-key? (hash-ref no-version 'versions) version)
          (hash? (hash-ref (hash-ref no-version 'versions) version #f)))
     =>
     (λ (version-ht)
       (hash-merge version-ht no-version))]
    [else
     no-version]))

(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (λ ()
              (match key
                [(or 'author 'source)
                 (error 'pkg "Package ~e is missing a required field: ~e"
                        (hash-ref pkg-info 'name) key)]
                ['checksum
                 ""]
                ['ring
                 2]
                ['checksum-error
                 #f]
                ['tags
                 empty]
                ['versions
                 (hash)]
                [(or 'last-checked 'last-edit 'last-updated)
                 -inf.0]))))

(define (package-info-set! pkg-name i)
  (write-to-file i (build-path pkgs-path pkg-name)
                 #:exists 'replace))

(define (hash-merge from to)
  (for/fold ([to to])
      ([(k v) (in-hash from)])
    (hash-set to k v)))

(define (author->list as)
  (string-split as))

(define (valid-name? t)
  (not (regexp-match #rx"[^a-zA-Z0-9_\\-]" t)))

(define (valid-author? a)
  (not (regexp-match #rx"[ :]" a)))

(define valid-tag?
  valid-name?)

(define (log! . args)
  (parameterize ([date-display-format 'iso-8601])
    (printf "~a: ~a\n" (date->string (current-date) #t) 
            (apply format args))))

(define (run! f args)
  (log! "START ~a ~v" f args)
  (f args)
  (log! "END ~a ~v" f args))

(define (safe-run! run-sema t)
  (thread 
   (λ () 
     (call-with-semaphore run-sema 
       (λ () 
         (with-handlers ([exn:fail? (λ (x) ((error-display-handler) 
                                            (exn-message x)
                                            x))])
           (t)))))))

(define s3-config (build-path (find-system-path 'home-dir) ".s3cfg-plt"))
(define s3-bucket "pkgs.racket-lang.org")

(define s3cmd-path (find-executable-path "s3cmd"))

(provide (all-defined-out))
