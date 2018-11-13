#!/bin/sh
#| -*- mode: scheme -*-
exec csi -s "$0" "$@"
|#

(module egg2nix

()

(import scheme)
(import matchable)

(import chicken.base)
(import chicken.file)
(import chicken.format)
(import chicken.io)
(import chicken.irregex)
(import chicken.port)
(import chicken.process-context)
(import chicken.sort)
(import chicken.string)
(import chicken.file.posix)
(import chicken.process)
(import chicken.pathname)

(import srfi-1)
(import args)

(define chicken-egg-builtins
  '(scheme
    r4rs
    r5rs
    srfi-4))

(define verbose? #f)

(define (info msg . args)
  (when verbose?
    (apply fprintf (current-error-port) msg args)
    (newline (current-error-port))
    (flush-output (current-error-port))))

(define *cache-dir*
  (make-pathname
   (current-directory)
   (create-directory ".egg2nix-cache-5")))

(define *temp-dir*
  (create-directory
   (string-append *cache-dir* "/eggs/")))

(set-environment-variable! "CHICKEN_EGG_CACHE" *temp-dir*)

(define-record-type egg
  (%make-egg name name-string version deps extra-deps broken?)
  egg?
  (name egg-name)
  (name-string egg-name-string)
  (version egg-version egg-version-set!)
  (deps egg-deps egg-deps-set!)
  (extra-deps egg-extra-deps)
  (broken? egg-broken?))

(define (make-egg name #!optional version extra-deps broken?)
  (let ((version (or version (latest-version name))))
    (%make-egg name (symbol->string name) version #f extra-deps broken?)))

(define-record-printer (egg egg out)
  (display "#<egg " out)
  (display (egg-name egg) out)
  (and-let* ((version (egg-version egg)))
    (display " " out)
    (write version out))
  (display ">" out))

(define known-versions
  (let ((f (make-pathname *cache-dir* "known-versions")))
    (info "Trying to read cached versions from ~s" f)
    (if (file-exists? f)
        (call-with-input-file f read)
        '())))

(define (all-versions egg-name)
  (or (alist-ref egg-name known-versions)
      ;; TODO: Add error handling
      (begin
        (info "Retrieving versions of ~a" egg-name)
        (receive (in out pid)
            (process "chicken-install"
                     (list "-list-versions" (symbol->string egg-name)))
          (let ((versions (cdr (string-split (read-line in) " "))))
            (close-input-port out)
            (receive (_ normal-exit? exit-code)
                (process-wait pid)
              (if (and normal-exit? (zero? exit-code))
                  (begin
                    (set! known-versions
                      (cons (cons egg-name versions) known-versions))
                    versions)
                  (error "list-versions failed"))))))))

(define (version>=? v1 v2)
  (define (version->list v)
    (map (lambda (x) (or (string->number x) x))
	       (irregex-split "[-\\._]" (->string v))))
  (let loop ((p1 (version->list v1))
	           (p2 (version->list v2)))
    (cond ((null? p1) (null? p2))
	        ((null? p2))
	        ((number? (car p1))
	         (and (number? (car p2))
		            (or (> (car p1) (car p2))
		                (and (= (car p1) (car p2))
			                   (loop (cdr p1) (cdr p2))))))
	        ((number? (car p2)))
	        ((string>? (car p1) (car p2)))
	        (else
	         (and (string=? (car p1) (car p2))
		            (loop (cdr p1) (cdr p2)))))))

(define (latest-version egg-name)
  (and-let* ((versions (all-versions egg-name))
             ((pair? versions)))
    (car (sort versions version>=?))))

(define (local-egg-path egg)
  (string-append *temp-dir*
                 (egg-name-string egg)
                 "-" (or (egg-version egg)
                         (latest-version (egg-name egg)))
                 "/" ))

(define egg-meta-files
  '("STATUS" "TIMESTAMP"))

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
                          (let ((dir (string-append name "-" version)))
                            (when (directory-exists? dir)
                                  (delete-directory dir #t))
                            (rename-file name dir)
                            (map (lambda (n)
                                   (delete-file
                                    (make-pathname dir n)))
                                 egg-meta-files))
                          #t)))))

(define (nix-hash egg)
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
      hash)))

(define (retrieve-egg egg)
  (change-directory *temp-dir*)
  (unless (directory? (local-egg-path egg))
    (chicken-install-retrieve (egg-name-string egg) (egg-version egg))))

(define (egg-meta egg)
  (let ((meta-file (make-pathname (local-egg-path egg)
                                  (egg-name-string egg)
                                  "egg")))
    (call-with-input-file meta-file read)))

(define (egg-ref name eggs)
  (find (lambda (egg)
          (eqv? name (egg-name egg)))
        eggs))

(define (egg-dependencies egg)
  (let ((meta (egg-meta egg)))
    (remove (lambda (dep) (member dep chicken-egg-builtins))
            (map spec-name
                 (or (alist-ref 'dependencies meta) '())))))

(define (all-dependencies egg eggs)
  (retrieve-egg egg)
  (let ((deps (egg-dependencies egg)))
    (egg-deps-set! egg deps)
    (fold (lambda (dep-name eggs)
            (let ((dep (egg-ref dep-name eggs)))
              (if dep
                  eggs
                  (let ((dep (make-egg dep-name)))
                    (all-dependencies dep (cons dep eggs))))))
          eggs
          deps)))

(define (egg-name->nix-attr-name name)
  (let* ((name (symbol->string name))
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
   (string-intersperse (map egg-name->nix-attr-name deps)
                       "\n      ")
   "\n    ]"))

(define (spec-extra-deps spec)
  (or (and (pair? spec)
           (alist-ref 'extra-dependencies (cdr spec)))
      '()))

(define (spec-broken? entry)
  (and (pair? entry)
       (alist-ref 'broken (cdr entry))))

(define (spec-version spec)
  (and (pair? spec)
       (car (or (alist-ref 'version (cdr spec))
                '(#f)))))

(define (spec-name spec)
  (if (symbol? spec)
      spec
      (car spec)))

(define (spec->egg spec)
  (unless (or (pair? spec) (symbol? spec))
    (error "Invalid egg spec" spec))
  (let* ((name (spec-name spec))
         (version (spec-version spec))
         (version (if version
                      (let ((versions (all-versions name)))
                        (if (member version versions)
                            version
                            (error "Invalid egg version" spec versions)))
                      (latest-version name)))
         (extra-deps (spec-extra-deps spec))
         (broken? (spec-broken? spec)))
    (make-egg name version extra-deps broken?)))

(define (specs->eggs specs)
  (fold (lambda (spec eggs)
          (let* ((egg (spec->egg spec))
                 (existing (egg-ref (egg-name egg) eggs)))
            (cond (existing
                   (assert (equal? (egg-version egg) (egg-version existing))
                           "Duplicate dependency with incompatible versions"
                           egg
                           existing)
                   eggs)
                  (else
                   (cons egg eggs)))))
        '()
        specs))

(define (egg<? a b)
  (string<? (egg-name-string a) (egg-name-string b)))

(define (collect-deps eggs)
  (sort (fold all-dependencies eggs eggs) egg<?))

(define (write-nix-expression egg)
  (let* ((name (egg-name egg))
         (version (egg-version egg))
         (deps (or (egg-deps egg) '()))
         (extra-deps (or (egg-extra-deps egg) '()))
         (hash (nix-hash egg))
         (broken? (egg-broken? egg)))
    (printf
     "
  ~A = eggDerivation {
    name = \"~A-~A\";

    src = fetchegg {
      name = \"~A\";
      version = \"~A\";
      sha256 = \"~A\";
    };

    buildInputs = ~A;~A
  };
"
     (egg-name->nix-attr-name name)
     name
     version
     name
     version
     hash
     (nix-deps-string (append deps extra-deps))
     (if broken? "\n    meta.broken = true;" ""))))

(define (write-nix-file specs)
  (let* ((eggs (specs->eggs specs))
         (eggs (collect-deps eggs)))
    (print "{ pkgs, stdenv }:")
    (print "rec {")
    (print "  inherit (pkgs) eggDerivation fetchegg;")
    (for-each write-nix-expression eggs)
    (print "}\n")

    ;; Write out version-cache
    (let ((f (make-pathname *cache-dir* "known-versions")))
      (call-with-output-file f
        (lambda (out)
          (info "writing versions to cache at ~s" f)
          (write known-versions out))))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [-v] input.scm > output.nix")
      (newline)
      (print (args:usage opts))
      (print "Report bugs to moritz@tarn-vedra.de")))
  (exit 1))

(define opts
  (list (args:make-option
         (v verbose) #:none "Verbose logging"
         (set! verbose? #t))
        (args:make-option
         (h help) #:none "Display this text"
         (usage))))

(receive (options operands)
         (args:parse (command-line-arguments) opts)
         (match operands
                (("-")
                 (write-nix-file (read-list)))
                ((file)
                 (write-nix-file (call-with-input-file file read-list)))
                (_
                 (usage))))



)
