#lang racket/base
(require racket/runtime-path)
(require net/url)
(require web-server/http)
(require "../main.rkt")
(define-runtime-path here ".")
(define var (getenv "PKGSERVER_DATADIR"))
(main (hash 'static-path (build-path var "public_html/pkg-index-static")
            'root (build-path var "pkg-index")
            'email-sender-address "The Racket Package Server <pkg@racket-lang.org>"

            'atom-package-url-format-string "http://pkgo.racket-lang.org/#[~a]"
            's3-bucket "pkgo.racket-lang.org"

            ;; 'atom-package-url-format-string "http://pkg.racket-lang.org/package/~a"
            ;; 's3-bucket "pkgn.racket-lang.org"
            ))
