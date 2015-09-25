#lang racket/base

(provide go)

(require web-server/http
         "common.rkt"
         "update.rkt"
         "notify.rkt"
         "static.rkt"
         "build-update.rkt"
         "jsonp.rkt"
         web-server/servlet-env
         racket/file
         web-server/dispatch
         racket/match
         racket/string
         net/url
         racket/list
         net/sendmail
         "../basic/main.rkt"
         file/sha1
         (prefix-in bcrypt- bcrypt)
         version/utils
         racket/set
         json)
(module+ test
  (require rackunit))

(define (package-remove! pkg-name)
  (delete-file (build-path^ pkgs-path pkg-name)))

(define (package-exists? pkg-name)
  (file-exists? (build-path^ pkgs-path pkg-name)))

(define (hash-deep-merge ht more-ht)
  (for/fold ([ht ht])
      ([(k new-v) (in-hash more-ht)])
    (hash-update ht k
                 (λ (old-v)
                   (cond
                     [(not old-v)
                      new-v]
                     [(hash? old-v)
                      (hash-deep-merge old-v new-v)]
                     [else
                      new-v]))
                 #f)))
(module+ test
  (check-equal?
   (hash-deep-merge (hasheq 'source "http://aws" 'descript "DrRacket")
                    (hasheq 'source "http://github"))
   (hasheq 'source "http://github" 'descript "DrRacket")))

(define (curation-administrator? u)
  (member u '("jay.mccarthy@gmail.com"
              "mflatt@cs.utah.edu"
              "samth@ccs.neu.edu"
              "stamourv@racket-lang.org")))

(define (api/upload req)
  (define req-data (read (open-input-bytes (or (request-post-data/raw req) #""))))
  (match-define (list email given-password pis) req-data)
  (define password-path (build-path^ users.new-path email))
  (define expected-password (file->bytes password-path))
  (define password-okay? (bcrypt-check expected-password given-password))
  (define curator? (curation-administrator? email))
  (cond
    [(not (and password-okay? curator?))
     (log! "api/upload! failed pass(~a) curator(~a) email was ~v"
           password-okay?
           curator?
           email)
     (response/sexpr #f)]
    [else
     (log! "receiving api/upload!")
     (for ([(p more-pi) (in-hash pis)])
       (log! "received api/upload for ~a" p)
       (define pi 
         (if (package-exists? p)
             (package-info p)
             #hash()))
       (define new-pi (hash-deep-merge pi more-pi))
       (define updated-pi
         (hash-remove
          (let ([now (current-seconds)])
            (for/fold ([pi new-pi])
                      ([k (in-list '(last-edit last-checked last-updated))])
              (hash-set pi k now)))
          'checksum))
       (log! "api/upload old ~v more ~v new ~v updated ~v"
             (hash-ref pi 'source #f)
             (hash-ref more-pi 'source #f)
             (hash-ref new-pi 'source #f)
             (hash-ref updated-pi 'source #f))
       (package-info-set! p updated-pi))
     (signal-update! (hash-keys pis))
     (response/sexpr #t)]))

(define redirect-to-static
  (get-config redirect-to-static-proc
              (lambda (req)
                (redirect-to
                 (url->string
                  (struct-copy url (request-uri req)
                               [scheme (get-config redirect-to-static-scheme "http")]
                               [host (get-config redirect-to-static-host "pkgs.racket-lang.org")]
                               [port (get-config redirect-to-static-port 80)]))))))

(define-syntax-rule (define-jsonp/auth (f . pat) . body)
  (define-jsonp
    (f
     ['email email]
     ['passwd passwd]
     . pat)
    (ensure-authenticate email passwd (λ () . body))))

(define (salty str)
  (sha1 (open-input-string str)))

(define current-user (make-parameter #f))
(define (ensure-authenticate email passwd body-fun)
  (define passwd-path (build-path^ users.new-path email))

  (define (authenticated!)
    (parameterize ([current-user email])
      (body-fun)))

  (cond
    [(not (file-exists? passwd-path))
     "new-user"]
    [(not (bcrypt-check (file->bytes passwd-path)
                        (string->bytes/utf-8 passwd)))
     "failed"]
    [else
     (authenticated!)]))

;; email-codes: (Hashtable String String)
;; Key: email address.
;; Value: code sent out in email and required for registration of a new password.
(define email-codes (make-hash))
;; TODO: Expire codes.

(define-jsonp
  (jsonp/authenticate
   ['email email]
   ['passwd passwd]
   ['code email-code])

  (define passwd-path (build-path^ users.new-path email))

  (define (generate-a-code email)
    (define correct-email-code
      (bytes->hex-string (with-input-from-file "/dev/urandom" (lambda () (read-bytes 12)))))
    (hash-set! email-codes email correct-email-code)
    correct-email-code)

  (define (codes-equal? a b)
    ;; Compare case-insensitively since people are weird and might
    ;; type the thing in.
    (string-ci=? a b))

  (define (check-code-or true false)
    (cond
      [(and (not (string=? "" email-code))
            (hash-ref email-codes email #f))
       => (λ (correct-email-code)
            (cond
              [(codes-equal? correct-email-code email-code)
               (display-to-file (bcrypt-encode (string->bytes/utf-8 passwd))
                                passwd-path
                                #:exists 'replace)

               (hash-remove! email-codes email)

               (true)]
              [else
               "wrong-code"]))]
      [else
       (false)

       #f]))

  (match (ensure-authenticate email passwd (λ () #t))
    ["failed"
     (check-code-or
      (λ () (hasheq 'curation (curation-administrator? email)))
      (λ ()
        (send-mail-message
         (get-config email-sender-address "pkg@racket-lang.org")
         "Account password reset for Racket Package Catalog"
         (list email)
         empty empty
         (list
          "Someone tried to login with your email address for an account on the Racket Package Catalog, but failed."
          "If you this was you, please use this code to reset your password:"
          ""
          (generate-a-code email)
          ""
          "This code will expire, so if it is not available, you'll have to try to again."))))]
    ["new-user"
     (check-code-or
      (λ () #t)
      (λ ()
        (send-mail-message
         (get-config email-sender-address "pkg@racket-lang.org")
         "Account confirmation for Racket Package Catalog"
         (list email)
         empty empty
         (list
          "Someone tried to register your email address for an account on the Racket Package Catalog."
          "If you want to proceed, use this code:"
          ""
          (generate-a-code email)
          ""
          "This code will expire, so if it is not available, you'll have to try to register again."))))]
    [#t
     (hasheq 'curation (curation-administrator? email))]))

(define-jsonp/auth
  (jsonp/package/modify
   ['pkg pkg]
   ['name mn-name]
   ['description mn-desc]
   ['source mn-source])
  (save-package! #:old-name pkg
                 #:new-name mn-name
                 #:description mn-desc
                 #:source mn-source
                 #:tags #f
                 #:authors #f
                 #:versions #f))

(define (jsonp/package/modify-all req)
  (define-jsonp/auth
    (inner//jsonp/package/modify-all)
    (define req-data (read-json (open-input-bytes (or (request-post-data/raw req) #""))))
    (and (hash? req-data)
         (let ((pkg (hash-ref req-data 'pkg #f))
               (name (hash-ref req-data 'name #f))
               (description (hash-ref req-data 'description #f))
               (source (hash-ref req-data 'source #f))
               (tags (hash-ref req-data 'tags #f))
               (authors (hash-ref req-data 'authors #f))
               (versions (hash-ref req-data 'versions #f)))
           (and (string? pkg)
                (string? name)
                (string? description)
                (string? source)
                (or (not tags) (and (list? tags) (andmap valid-tag? tags)))
                (or (not authors) (and (list? authors) (andmap valid-author? authors)))
                (or (not versions) (and (list? versions) (andmap valid-versions-list-entry? versions)))
                (save-package! #:old-name pkg
                               #:new-name name
                               #:description description
                               #:source source
                               #:tags tags
                               #:authors authors
                               #:versions versions)))))
  (inner//jsonp/package/modify-all req))

(define (valid-versions-list-entry? entry)
  (and (pair? entry)
       (pair? (cdr entry))
       (null? (cddr entry))
       (valid-version? (car entry))
       (string? (cadr entry))))

;; Call ONLY within scope of an ensure-authenticate!
(define (save-package! #:old-name old-name
                       #:new-name new-name
                       #:description description
                       #:source source
                       #:tags tags0
                       #:authors authors0
                       #:versions versions0)
  (when (not (current-user)) (error 'save-package! "No current-user"))
  (define new-package? (equal? old-name ""))
  (define (do-save! base-hash)
    (let* ((h base-hash)
           (h (cond [authors0
                     (hash-set h
                               'author
                               (string-join
                                (set->list (set-add (list->set authors0) (current-user)))))]
                    [new-package?
                     (hash-set h 'author (current-user))]
                    [else
                     h]))
           (h (if tags0 (hash-set h 'tags (tags-normalize tags0)) h))
           (h (if versions0
                  (hash-set h 'versions (for/hash [(v versions0)] (values (car v) (cadr v))))
                  h))
           (h (hash-set h 'name new-name))
           (h (hash-set h 'source source))
           (h (hash-set h 'description description))
           (h (hash-set h 'last-edit (current-seconds))))
      (package-info-set! new-name h)))
  (cond
    [(not (andmap valid-author? (or authors0 '()))) #f]
    [(not (andmap valid-tag? (or tags0 '()))) #f]
    [(not (andmap valid-versions-list-entry? (or versions0 '()))) #f]
    [new-package?
     (cond
      [(or (package-exists? new-name)
           (not (valid-name? new-name)))
       #f]
      [else
       (do-save! (hasheq))
       (signal-update! (list new-name))
       #t])]
    [else
     (ensure-package-author
      old-name
      (λ ()
        (cond
          [(equal? new-name old-name)
           (do-save! (package-info old-name))
           (signal-update! (list new-name))
           #t]
          [(and (valid-name? new-name)
                (not (package-exists? new-name)))
           (do-save! (package-info old-name))
           (package-remove! old-name)
           (signal-update! (list new-name))
           #t]
          [else
           #f])))]))

(define-jsonp/auth
  (jsonp/package/version/add
   ['pkg pkg]
   ['version version]
   ['source source])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(valid-version? version)
        (package-info-set!
         pkg
         (hash-update (package-info pkg) 'versions
                      (λ (v-ht)
                        (hash-set v-ht version
                                  (hasheq 'source source
                                          'checksum "")))
                      hash))
        (signal-update! (list pkg))
        #t]
       [else
        #f]))))

(define-jsonp/auth
  (jsonp/package/version/del
   ['pkg pkg]
   ['version version])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(valid-version? version)
        (package-info-set!
         pkg
         (hash-update (package-info pkg) 'versions
                      (λ (v-ht)
                        (hash-remove v-ht version))
                      hash))
        (signal-update! (list pkg))
        #t]
       [else
        #f]))))

(define (tags-normalize ts)
  (remove-duplicates (sort ts string-ci<?)))

(define-jsonp/auth
  (jsonp/package/tag/add
   ['pkg pkg]
   ['tag tag])
  (cond
    [(valid-tag? tag)
     (define i (package-info pkg))
     (package-info-set!
      pkg
      (hash-set i 'tags (tags-normalize (cons tag (package-ref i 'tags)))))
     (signal-static! (list pkg))
     #t]
    [else
     #f]))

(define-jsonp/auth
  (jsonp/package/tag/del
   ['pkg pkg]
   ['tag tag])
  (ensure-package-author
   pkg
   (λ ()
     (define i (package-info pkg))
     (package-info-set!
      pkg
      (hash-set i 'tags
                (remove tag
                        (package-ref i 'tags))))
     (signal-static! (list pkg))
     #t)))

(define-jsonp/auth
  (jsonp/package/author/add
   ['pkg pkg]
   ['author author])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(valid-author? author)
        (define i (package-info pkg))
        (package-info-set!
         pkg
         (hash-set i 'author (format "~a ~a" (package-ref i 'author) author)))
        (signal-static! (list pkg))
        #t]
       [else
        #f]))))

(define (ensure-package-author pkg f)
  (cond
    [(package-author? pkg (current-user))
     (f)]
    [else
     #f]))

(define-jsonp/auth
  (jsonp/package/author/del
   ['pkg pkg]
   ['author author])
  (ensure-package-author
   pkg
   (λ ()
     (cond
       [(not (equal? (current-user) author))
        (define i (package-info pkg))
        (package-info-set!
         pkg
         (hash-set i 'author
                   (string-join
                    (remove author
                            (author->list (package-ref i 'author))))))
        (signal-static! (list pkg))
        #t]
       [else
        #f]))))

(define-jsonp/auth
  (jsonp/package/del
   ['pkg pkg])
  (ensure-package-author
   pkg
   (λ ()
     (package-remove! pkg)
     (signal-static! empty)
     #f)))

(define-jsonp/auth
  (jsonp/package/curate
   ['pkg pkg]
   ['ring ring-s])
  (cond
    [(curation-administrator? (current-user))
     (define i (package-info pkg))
     (define ring-n (string->number ring-s))
     (package-info-set!
      pkg
      (hash-set i 'ring (min 2 (max 0 ring-n))))
     (signal-static! (list pkg))
     #t]
    [else
     #f]))

(define (package-author? p u)
  (define i (package-info p))
  (member u (author->list (package-ref i 'author))))

(define (packages-of u)
  (filter (λ (p) (package-author? p u)) (package-list)))

(define-jsonp/auth
  (jsonp/update)
  (signal-update! (packages-of (current-user)))
  #t)

(define jsonp/notice
  (make-jsonp-responder (λ (args) (file->string notice-path))))

(define-values (main-dispatch main-url)
  (dispatch-rules
   [("jsonp" "authenticate") jsonp/authenticate]
   [("jsonp" "update") jsonp/update]
   [("jsonp" "package" "del") jsonp/package/del]
   [("jsonp" "package" "modify") jsonp/package/modify]
   [("jsonp" "package" "modify-all") #:method "post" jsonp/package/modify-all]
   [("jsonp" "package" "version" "add") jsonp/package/version/add]
   [("jsonp" "package" "version" "del") jsonp/package/version/del]
   [("jsonp" "package" "tag" "add") jsonp/package/tag/add]
   [("jsonp" "package" "tag" "del") jsonp/package/tag/del]
   [("jsonp" "package" "author" "add") jsonp/package/author/add]
   [("jsonp" "package" "author" "del") jsonp/package/author/del]
   [("jsonp" "package" "curate") jsonp/package/curate]
   [("api" "upload") #:method "post" api/upload]
   [("jsonp" "notice") jsonp/notice]
   [else redirect-to-static]))

(define-syntax-rule (forever . body)
  (let loop () (begin . body) (loop)))

(define (go)
  (define port (get-config port 9004))
  (log! "launching on port ~v" port)
  (signal-static! empty)
  (thread
   (λ ()
     (forever
      (log! "update-t: Running scheduled build update.")
      (signal-build-update!)
      (log! "update-t: Running scheduled update.")
      (signal-update!/beat empty)
      (log! "update-t: sleeping for 1 hour")
      (sleep (* 1 60 60)))))
  (serve/servlet
   main-dispatch
   #:command-line? #t
   ;; xxx I am getting strange behavior on some connections... maybe
   ;; this will help?
   #:connection-close? #t
   #:listen-ip #f
   #:ssl? #t
   #:ssl-cert (build-path root "server-cert.pem")
   #:ssl-key (build-path root "private-key.pem")
   #:extra-files-paths empty
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go))
