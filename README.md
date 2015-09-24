# Package Catalog Server

Common configuration keys:

 - `src` - path; defaults to this package's `official/.` directory.

	- `static.src-path` - path; defaults to *src*`/static`. Location
      of (non-generated) HTML/JS/CSS files to be copied to
      `static-path` (see below).

    - `static-path` - path; defaults to *src*`/static-gen`. Staging
	  area where all static resources - both generated and
	  non-generated - are copied to before being copied in turn to
	  whereever static content is to be served from (usually an S3
	  bucket).

    - `notice-path` - path; defaults to *static-path*`/notice.json`.
      Whenever the server has a message for site users, the message
      will be placed in this file.

 - `root` - path; defaults to this package's `official/root`
   directory. Used as a base for many other configuration variables:

    - `users.new-path` - path; defaults to *root*`/users.new`.
      Directory in which to hold user records, one file per user.

    - `github-client_id` - string; defaults to the contents of the
      file at *root*`/client_id`. Should be a Github client ID string
      (hex; twenty characters long, i.e. 10 bytes of data,
      hex-encoded)

    - `github-client_secret` - string; defaults to the contents of the
      file at *root*`/client_secret`. Should be a Github client secret
      string (hex; forty characters long, i.e. 20 bytes of data,
      hex-encoded)

    - `cache-path` - path; defaults to *root*`/cache`. Names a
      directory where files `summary.rktd` and `summary.rktd.etag`
      will be created.

	- `pkgs-path` - path; defaults to *root*`/pkgs`. Names a directory
      where one file of package information for each package in the
      catalog will be stored.

 - `s3-config` - path; defaults to the contents of the environment
   variable `S3CFG_PLT`, if it is defined; otherwise, to the file
   `.s3cfg-plt` in the user's home directory (usually `$HOME`). The
   file at this location should be a configuration file for `s3cmd`;
   see also the configuration key `s3cmd-path` below.

 - `s3-bucket` - string; defaults to the contents of the environment
   variable `S3_BUCKET`, if it is defined; otherwise, to
   `pkgs.racket-lang.org`.

 - `s3cmd-path` - path; defaults to the contents of the environment
   variable `S3CMD`, if it is defined; otherwise, to the full path to
   the `s3cmd` executable on the system executable path.

Configuration keys used by `dynamic.rkt`:

 - `redirect-to-static-proc` - function from HTTP request to HTTP
   response, which should issue a redirect pointing to a static
   resource. Defaults to a function which replaces the scheme with the
   contents of the configuration variable `redirect-to-static-scheme`,
   the host with `redirect-to-static-host`, and the port to
   `redirect-to-static-port`. These, in turn, default to `"http"`,
   `"pkgs.racket-lang.org"` and `80`, respectively.

 - `email-sender-address` - string; defaults to `pkg@racket-lang.org`.
   Used as the "from" address when sending authentication emails on
   behalf of the server.

 - `port` - number; defaults to `9004`. Port on which the backend site
   will be served (N.B. via HTTPS).

Configuration keys used by `static.rkt`:

 - `atom-self-link` - string; defaults to
   `https://pkg.racket-lang.org/rss`. Used as the `rel=self` link in
   the header of the generated ATOM feed.

 - `atom-link` - string; defaults to `https://pkg.racket-lang.org/`.
   Used as the default site link in the header of the generated ATOM
   feed.

 - `atom-id` - string; defaults to `https://pkg.racket-lang.org/`.
   Used as the ATOM feed ID.

 - `atom-compute-package-url` - function from package-name symbol to
   URL string. Defaults to a function which calls `format` with the
   package name and a format template-string from
   `atom-package-url-format-string`, which in turn defaults to
   `http://pkg.racket-lang.org/#[~a]`.
