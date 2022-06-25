# Package Catalog Server

## Dependencies

Run `raco pkg install` to install all dependencies

## Running

Run the dynamic server via `main.rkt`. Provide the `--config` file to
specify a module that exports `config` as a hash table with symbol
keys to configure the server. Alternatively, run one of the modules in
`official/configs/`; those modules are also useful as configuration
examples.

To initialize the set of served packages, use `raco pkg catalog-copy`
to copy an existing catalog to directory form, and then move the `pkg`
directory in the catalog copy to be *root*`/pkgs` (where *root* is the
server's root directory).

## Configuration

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

    - `cache-path` - path; defaults to *root*`/cache`. Names a
      directory where files `summary.rktd` and `summary.rktd.etag`
      will be created.

    - `pkgs-path` - path; defaults to *root*`/pkgs`. Names a directory
      where one file of package information for each package in the
      catalog will be stored.

    - `github-client_id` (obsolete) - string #f; defaults to the contents of the
      file at *root*`/client_id`, if it exists. Should be a Github client ID string
      (hex; twenty characters long, i.e. 10 bytes of data,
      hex-encoded), used only if package downloaing is forced to use the
      GitHub API by setting the `PLT_USE_GITHUB_API` environment variable

    - `github-client_secret` (obsolete) - string or #f; defaults to the contents of the
      file at *root*`/client_secret`, if it exists. Should be a Github client secret
      string (hex; forty characters long, i.e. 20 bytes of data,
      hex-encoded), used only when `github-client_id` is used

 - `s3-bucket` - string or `#f`; defaults to the contents of the
   environment variable `S3_BUCKET`, if it is defined; otherwise, to
   `test.racket-lang.org`. AWS credentials are found by the `s3`
   package, typically from `~/.aws-keys`. If set to `#f`, S3
   synchronization will be disabled.

 - `s3-bucket-region` - string; defaults to the contents of the
   environment variable `S3_BUCKET_REGION`, if it is defined;
   otherwise, to `us-east-1`.

 - `beat-s3-bucket` - string or #f; defaults to #f. A bucket name for
   regsitering heartbeats, or #f to disable heartbeats.

Configuration keys used by `dynamic.rkt`:

 - `redirect-to-static-proc` - function from HTTP request to HTTP
   response, which should issue a redirect pointing to a static
   resource. Defaults to a function which replaces the scheme with the
   contents of the configuration variable `redirect-to-static-scheme`,
   the host with `redirect-to-static-host`, and the port to
   `redirect-to-static-port`. These, in turn, default to `"http"`,
   `"pkgs.racket-lang.org"` and `80`, respectively.

 - `port` - number; defaults to `9004`. Port on which the backend site
   will be served (N.B. via HTTPS).

 - `ssl?` - boolean; ; defaults to `#t`. A true value serves HTTPS and
    requires *root*/`server-cert.pem` and *root*/`private-key.pem`.

 - `beat-update-task-name` - string; defaults to "pkgd-update". A task
   name for heartbeats after updating information for all packages.

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

 - `beat-upload-task-name` - string; defaults to "pkgd-upload". A task
   name for heartbeats after uploading information for all packages.


## Example setup and run

Run:

```
$ raco pkg catalog-copy https://pkgs.racket-lang.org pkgs-copy
```

to initialize the set of served packages to those in the package server.

Next, create the directory `root` under `official`

```
$ mkdir official/root
```

Next, copy the packages to `root`:

```
$ cp -r pkgs-copy/pkg official/root/pkgs
```

Now, run `racket official/configs/tonyg.rkt`. The outputs will be in the directory `official/static-gen`.
