#lang racket/base
(require racket/runtime-path)
(require net/url)
(require web-server/http)
(require "../main.rkt")
(define-runtime-path here ".")
(main (hash 's3cmd-path (build-path here "private/tonyg-s3cmd.py")
            'redirect-to-static-proc
            (lambda (req)
              (log-info "Would redirect to static version of ~a"
                        (url->string (request-uri req)))
              (redirect-to "http://localhost/~tonyg/pkg-catalog-static/FIXME"))
            'email-sender-address "tonyg@racket-lang.org"
            'port 8445
            'atom-package-url-format-string
              "https://localhost/~~tonyg/pkg-catalog-static/package/~a"
            ))
