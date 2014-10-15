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
  (map symbol->string '(scheme
                        r4rs
                        r5rs
                        chicken
                        extras
                        data-structures
                        ports
                        lolevel
                        posix
                        files
                        ;; Note: This is included in newer chickens?
                        ;; regex
                        srfi-1
                        srfi-4
                        srfi-13
                        srfi-14
                        srfi-18
                        srfi-69
                        tcp
                        utils)))

(define (temp-directory)
  (let ((dir "/tmp/egg2nix/"))
    (unless (directory? dir)
      (create-directory dir #t))
    dir))

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
  (uri-reference "http://code.call-cc.org/cgi-bin/henrietta.cgi"))

(define known-versions '())

(define (all-versions egg-name)
  (or (alist-ref egg-name known-versions equal?)
      (parameterize ((form-urlencoded-separator "&"))
        ;; TODO: Add error handling
        (call-with-input-request
         (update-uri henrietta-uri query: `((name . ,egg-name)
                                            (listversions . 1)))
         #f
         (lambda (in)
           (let ((versions (string-split (read-string #f in) "\n")))
             (set! known-versions (cons (cons egg-name versions) known-versions))
             versions))))))

(define (latest-version egg)
  (and-let* ((versions (all-versions (egg-name egg)))
             ((pair? versions)))
    (first (sort versions version>=?))))

(define (update-version egg #!optional version)
  (list (string->symbol (egg-name egg)) (or version (latest-version egg))))

(define (local-egg-path egg)
  (string-append (temp-directory) (egg-name egg) "/"))

(define (chicken-install-retrieve egg)
  (let ((name (egg-name egg))
        (version (egg-version egg)))
    (receive (in out pid)
      (process "chicken-install"
               (list "-retrieve"
                     (if version
                         (string-append name ":" version)
                         name)))
      (receive (_ normal-exit? exit-code)
        (process-wait pid)
        (close-input-port in)
        (and normal-exit? (zero? exit-code))))))

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

(define (retrieve-egg egg)
  (change-directory (temp-directory))
  ;; Check if `version' is a valid release.
  (let ((name (egg-name egg))
        (version (egg-version egg)))
    (when version
      (assert (member version (all-versions name))
              (format "Version ~A is not a valid version." version)))
    (let ((path (local-egg-path egg)))
      (and (or (directory? path)
               (chicken-install-retrieve egg))
           path))))

(define (egg-meta egg)
  (let* ((name (egg-name egg))
         (meta-file (make-pathname (local-egg-path egg) name "meta")))
    (call-with-input-file meta-file read)))

(define (egg-dependencies egg)
  (let ((meta (egg-meta egg)))
    (remove (lambda (x) (member (egg-name x) chicken-egg-builtins))
            (append (or (alist-ref 'depends meta) '())
                    (or (alist-ref 'needs meta) '())))))


(define (egg-in-list? egg lst #!optional name-only)
  (any (if name-only
           (lambda (x) (string= (egg-name egg)
                                (egg-name x)))
           (lambda (x) (equal? egg x)))
       lst))


;;; Checks if the `eggs' contains an egg with the same name and a
;;; higher version. Does special handling of non-versioned eggs.
;;; (Those are always greater than the explicitly-versioned)
(define (egg-with-higher-version-in-list? egg eggs)
  (let* ((name (egg-name egg))
         (version (egg-version egg))
         (versions (map egg-version
                        (filter (lambda (x) (equal? name (egg-name x)))
                                (remove (cut equal? egg <>) eggs)))))
    (cond
     ((member #f versions) #t)
     ((eq? #f version) #f)
     (else (any (cut version>=? <> version) versions)))))

(define (remove-older-duplicate-eggs eggs)
  (delete-duplicates
   (remove (cut egg-with-higher-version-in-list? <> eggs) eggs)))

(define (all-dependencies egg #!optional acc ignore-version)
  (let* ((egg (if ignore-version (update-version egg) egg))
         (name (egg-name egg))
         (version (egg-version egg))
         (acc (or acc '())))
    ;; Only retrieves the same egg once
    (retrieve-egg egg)
    (let ((dependencies (egg-dependencies egg))
          ;; Add egg `name' to the accumulated list of deps
          (acc (cons egg acc)))
      ;; Leaf of the dep-tree
      (if (not dependencies)
          acc
          ;; Add all dependencies by folding over them. This
          ;; assures we don't load a dependency twice.
          (foldl (lambda (acc egg)
                   (all-dependencies egg acc #t))
                 acc
                 dependencies)))))

(define (nix-deps-string deps)
  (string-append
   "[\n      "
   (string-join (map egg-name deps)
                "\n      ")
   "\n    ]"))

(define (find-in-spec egg spec)
  (find (lambda (spec-entry)
          (equal? (egg-name egg)
                  (egg-name
                   (if (list? spec-entry)
                       (first spec-entry)
                       spec-entry))))
        spec))

(define (spec-native-dependencies entry)
  (and (list? entry)
       (alist-ref #:native-dependencies (cdr entry))))

(define (spec-version entry)
  (and (list? entry)
       (car (alist-ref #:version (cdr entry) eqv? '(#f)))))

(define (spec-name entry)
  (if (list? entry)
      (first entry)
      entry))

(define (spec->egg entry)
  (let ((name (spec-name entry))
        (version (spec-version entry)))
    (when version
      (assert (member version (all-versions (egg-name name)))))
    (if version
        (list name version)
        name)))

(define (egg<? a b)
  (string<? (egg-name a) (egg-name b)))

(define (collect-eggs spec)
  (let* ((eggs (map spec->egg spec))
         (deps (append-map (lambda (egg)
                             (all-dependencies egg eggs #f))
                           eggs))
         (deps (remove-older-duplicate-eggs deps)))
    (sort deps egg<?)))

(define (write-nix-expression egg spec)
  (let* ((spec-entry (find-in-spec egg spec))
         (native-deps (spec-native-dependencies spec-entry))
         (name (egg-name egg))
         (version (or (egg-version egg)
                      (latest-version name)))
         ;; (version (spec-version spec-entry))
         (hash (nix-hash egg))
         (deps (egg-dependencies egg)))
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
     name
     name
     version
     name
     version
     hash
     (nix-deps-string (append deps (or native-deps '()))))))

(define (write-nix-file spec)
  (delete-directory (temp-directory) #t)
  (let* ((eggs (collect-eggs spec)))
    (print "{ pkgs, stdenv }:")
    (print "rec {")
    (print "inherit (pkgs) eggDerivation fetchegg;")
    (for-each (lambda (egg) (write-nix-expression egg spec)) eggs)
    (print "}\n")
    (delete-directory (temp-directory) #t)))

(match (command-line-arguments)
  ((file)
   (write-nix-file (read-file file)))
  (_
   (print "Usage: egg2nix input.scm > output.nix")))
