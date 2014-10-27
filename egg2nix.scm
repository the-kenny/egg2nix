#!/bin/sh
#| -*- mode: scheme -*-
exec csi -s "$0" "$@"
|#

(use posix)
(use extras)
(use srfi-1)
(use matchable)
(use http-client)
(use uri-common)
(use intarweb)
(use files)
(use data-structures)
(use setup-api)

(define chicken-egg-builtins
  '(scheme
    r4rs
    r5rs
    chicken
    extras
    data-structures
    ports
    irregex
    lolevel
    posix
    files
    foreign
    ;; Note: This is included in newer chickens?
    ;; regex
    srfi-1
    srfi-4
    srfi-13
    srfi-14
    srfi-18
    srfi-69
    tcp
    utils))

(define verbose? #f)

(define (info msg . args)
  (when verbose?
    (apply fprintf (current-error-port) msg args)
    (newline (current-error-port))
    (flush-output (current-error-port))))

(define *cache-dir* (make-pathname (current-directory)
                                   (create-directory ".egg2nix-cache")))

(define *temp-dir* (create-directory
                    (string-append *cache-dir* "/eggs/")))

(define (egg-name egg)
  (cond ((pair? egg)
         (symbol->string (first egg)))
        ((symbol? egg)
         (symbol->string egg))
        (else
         egg)))

(define (egg-version egg)
  (and (list? egg)
       (let ((v (second egg)))
         (if (number? v)
             (number->string v)
             v))))

(define henrietta-uri
  "http://code.call-cc.org/cgi-bin/henrietta.cgi")

(define known-versions (or
                        (let ((f (make-pathname *cache-dir* "known-versions")))
                          (info "Trying to read cached versions from ~s" f)
                          (and (file-exists? f) (call-with-input-file f read)))
                        '()))

(define (all-versions egg)
  (or (alist-ref egg known-versions)
      ;; TODO: Add error handling
      (begin
        (info "Retrieving versions of ~a" egg)
        (call-with-input-request
         ;; NOTE: We don't use proper URL encoding here because henrietta doesn't properly decode it either
         (uri-reference (string-append henrietta-uri "?name=" (symbol->string egg) "&listversions=1"))
         #f
         (lambda (in)
           (let ((versions (string-split (read-string #f in) "\n")))
             (set! known-versions (cons (cons egg versions) known-versions))
             versions))))))

(define (latest-version egg)
  (and-let* ((versions (all-versions egg))
             ((pair? versions)))
    (first (sort versions version>=?))))

(define (local-egg-path egg)
  (string-append *temp-dir*
                 (egg-name egg)
                 "-" (or (egg-version egg)
                         (latest-version egg))
                 "/" ))

(define (chicken-install-retrieve name version)
  (let ((name+version (if version
                          (string-append name ":" version)
                          name)))
    (info "Retrieving ~a" name+version)
    (receive (in out pid)
             (process "chicken-install" (list "-retrieve" name+version))
             (receive (_ normal-exit? exit-code)
                      (process-wait pid)
                      (close-input-port in)
                      (if (and normal-exit? (zero? exit-code))
                          (rename-file name (string-append name "-" version))
                          #t)))))

(define (nix-hash egg)
  (let ((name (egg-name egg)))
    (assert (directory? (local-egg-path egg)))
    (receive (in out pid)
      (process "nix-hash"
               (list "--base32"
                     "--type"
                     "sha256"
                     (local-egg-path egg)))
      (let ((hash (read-line in)))
        (close-input-port in)
        (close-output-port out)
        hash))))

(define (retrieve-egg egg version)
  (change-directory *temp-dir*)
  (let ((name (egg-name egg))
        (path (local-egg-path egg)))
    (and (or (directory? path)
             (begin
               ;; Check if `version' is a valid release.
               (assert (member version (all-versions egg))
                       (format "Version ~A is not a valid version." version))
               (chicken-install-retrieve name version)))
         path)))

(define (egg-meta egg)
  (let* ((name (egg-name egg))
         (meta-file (make-pathname (local-egg-path egg) name "meta")))
    (call-with-input-file meta-file read)))

(define (egg-dependencies egg)
  (let ((meta (egg-meta egg)))
    (remove (lambda (dep) (member dep chicken-egg-builtins))
            (map spec-name
                 (append (or (alist-ref 'depends meta) '())
                         (or (alist-ref 'needs meta) '()))))))

(define (all-dependencies egg spec given-deps)
  (if (alist-ref egg given-deps)
      given-deps
      (let* ((egg-spec (alist-ref egg spec))
             (version (or (spec-version egg-spec)
                          (latest-version egg))))
        ;; Only retrieves the same egg once
        (retrieve-egg egg version)
        (let ((deps (egg-dependencies egg)))
          (fold (lambda (dep given-deps)
                  (all-dependencies dep spec given-deps))
                (cons (cons* egg version deps)
                      given-deps)
                deps)))))

(define (egg-name->attrname egg)
  (let* ((name (egg-name egg))
         (name (irregex-replace/all "\\+" name "-plus"))
         (name (irregex-replace/all "^\\d+" name
                                    (lambda (m)
                                      (string-append
                                       (string-intersperse 
                                        (map (lambda (n)
                                               (vector-ref '#("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
                                                           (string->number (string n))))
                                             (string->list (irregex-match-substring m)))
                                        "-")
                                       "-")))))
    name))

(define (nix-deps-string deps)
  (string-append
   "[\n      "
   (string-join (map egg-name->attrname deps)
                "\n      ")
   "\n    ]"))

(define (spec-native-dependencies entry)
  (or (and (pair? entry)
           (alist-ref 'native-dependencies (cdr entry)))
      '()))

(define (spec-version entry)
  (and (pair? entry)
       (car (alist-ref 'version (cdr entry) eqv? '(#f)))))

(define (spec-name entry)
  (if (pair? entry)
      (car entry)
      entry))

(define (spec->egg entry)
  (let ((name (spec-name entry))
        (version (spec-version entry)))
    (when version
      (assert (member version (all-versions (egg-name name)))))
    (list name version)))

(define (normalize-spec spec)
  (fold (lambda (entry rspec)
          (let* ((name (spec-name entry))
                 (version (spec-version entry))
                 (existing (alist-ref name rspec)))
            (cond (existing
                   (assert (equal? version (spec-version existing))
                           "Duplicate dependency with incompatible versions"
                           entry
                           (cons name existing))
                   rspec)
                  (else
                   (cons (cons name
                               `((version ,version)
                                 ,@(if (pair? entry)
                                       (cdr entry)
                                       '())))
                         rspec)))))
        '()
        spec))

(define (egg<? a b)
  (string<? (egg-name a) (egg-name b)))

(define (collect-deps spec)
  (sort (fold (lambda (entry given-deps)
                (all-dependencies (car entry) spec given-deps))
              '()
              spec)
        egg<?))

(define (write-nix-expression egg+deps spec)
  (match-let* (((egg version deps ...) egg+deps)
               (egg-spec (assq egg spec))
               (native-deps (spec-native-dependencies egg-spec))
               (name (egg-name egg))
               (hash (nix-hash egg)))
    (printf
     "
  ~A = eggDerivation {
    name = \"~A-~A\";

    src = fetchegg {
      name = \"~A\";
      version = \"~A\";
      sha256 = \"~A\";
    };

    buildInputs = ~A;
  };
"
     (egg-name->attrname name)
     name
     version
     name
     version
     hash
     (nix-deps-string (append deps native-deps)))))

(define (write-nix-file spec)
  (let* ((spec (normalize-spec spec))
         (eggs+deps (collect-deps spec)))
    (print "{ pkgs, stdenv }:")
    (print "rec {")
    (print "  inherit (pkgs) eggDerivation fetchegg;")
    (for-each (lambda (egg+deps) (write-nix-expression egg+deps spec)) eggs+deps)
    (print "}\n")

    ;; Write out version-cache
    (let ((f (make-pathname *cache-dir* "known-versions")))
      (call-with-output-file f
        (lambda (out)
          (info "writing versions to cache at ~s" f)
          (write known-versions out))))))

(let loop ((args (command-line-arguments)))
  (match args
    (("-v" args ...)
     (set! verbose? #t)
     (loop args))
    (("-")
     (write-nix-file (read-file)))
    ((file)
     (write-nix-file (read-file file)))
    (_
     (print "Usage: egg2nix [-v] input.scm > output.nix"))))
