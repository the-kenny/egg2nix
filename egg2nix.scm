#! csi -script

(use posix)
(use extras)
(use srfi-1)
(use matchable)
(use http-client)
(use versions)

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

(define always-use-latest-version-for-deps #t)

(define (temp-directory)
  (let ((dir "/tmp/egg2nix/"))
    (if (not (directory? dir))
        (create-directory dir #t))
    dir))

(define (egg-name egg)
  (if (list? egg)
      (symbol->string (first egg))
      (if (string? egg) egg (symbol->string egg))))

(define (egg-version egg)
  (if (list? egg)
      (let ((v (second egg)))
       (if (number? v)
           (number->string v)
           v))
      #f))

(define known-versions '())

(define (all-versions egg-name)
  (let ((known (assoc egg-name known-versions)))
    (if known
        (cdr known)
        (begin
          (with-input-from-request
           (string-append "http://code.call-cc.org/cgi-bin/henrietta.cgi"
                          "?name=" egg-name
                          "&listversions=1")
           #f
           (lambda ()
             (let ((versions (string-split (read-string) "\n")))
               (set! known-versions (cons (cons egg-name versions) known-versions))
               versions)))))))

(define (latest-version egg)
  (first (version-sort (all-versions (egg-name egg)) #f)))

(define (update-version egg #!optional version)
  (list (string->symbol (egg-name egg)) (or version (latest-version egg))))

(define (local-egg-path egg)
  (string-append (temp-directory) (egg-name egg) "/"))

(define (chicken-install-retrieve egg)
  (let ((name (egg-name egg))
        (version (egg-version egg)))
    (let-values (((in out pid)
                  (process "chicken-install"
                           (list "-retrieve"
                                 (if version
                                     (string-append name ":" version)
                                     name)))))
      (let ((ret (process-wait pid)))
        (rename-file (string-append (temp-directory) name "/")
                     (local-egg-path egg))
        ret))))

(define (nix-hash egg)
  (let ((name (egg-name egg)))
    (assert (directory? (local-egg-path egg)))
    (let-values (((in out pid) (process "nix-hash"
                                        (list "--base32"
                                              "--type"
                                              "sha256"
                                              (local-egg-path egg)))))
      (string-trim-right (read-string #f in)))))

(define (retrieve-egg egg)
  (change-directory (temp-directory))
  ;; Check if `version' is a valid release.
  (let ((name (egg-name egg))
        (version (egg-version egg)))
    (if version
        (assert (member version (all-versions name))
                (format "Version ~A is not a valid version." version)))
    (let ((path (local-egg-path egg)))
      (if (not (directory? path))
          (let ((status (chicken-install-retrieve egg)))
            (if (= 0 status)
                path))
          path))))

(define (egg-meta egg)
  (let ((name (egg-name egg)))
    (car
     (read-file (string-append (local-egg-path egg)
                               name ".meta")))))

(define (egg-dependencies egg)
  (let ((meta (egg-meta egg)))
    (remove (lambda (x) (member (egg-name x) chicken-egg-builtins))
            (append (cdr (or (assq 'depends meta) '(())))
                    (cdr (or (assq 'needs meta) '(())))))))


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
     (#t (any (cut version-newer? <> version) versions)))))

(define (remove-older-duplicate-eggs eggs)
  (delete-duplicates
   (remove (cut egg-with-higher-version-in-list? <> eggs) eggs)))

(define (all-dependencies egg #!optional acc ignore-version)
  (let* ((egg (if ignore-version (update-version egg) egg))
         (name (egg-name egg))
         (version (egg-version egg))
         (acc (or acc '())))
    ;; Only retrieve the egg once
    (if (not (directory? (local-egg-path egg)))
        (retrieve-egg egg))
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
   "[\n    "
   (string-join (map egg-name deps)
                "\n    ")
   "\n  ]"))

(define (nix-expression egg #!optional native-deps)
  (let* ((name (egg-name egg))
         (version (or (egg-version egg)
                      (latest-version name)))
         (hash (nix-hash egg))
         (deps (egg-dependencies egg)))
    (format
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
    (if version
        (assert (member version (all-versions (egg-name name)))))
    (if version
        (list name version)
        name)))

(define (collect-eggs spec)
  (let* ((eggs (map spec->egg spec))
         (deps (map (lambda (egg)
                      (all-dependencies egg eggs #f))
                    eggs))
         (all (foldl append eggs deps)))
    (remove-older-duplicate-eggs all)))

(define (nix-file spec)
  (delete-directory (temp-directory) #t)
  (let* ((eggs (collect-eggs spec))
         (expressions (map (lambda (egg)
                             (let* ((spec-entry (find-in-spec egg spec))
                                    (native-deps (spec-native-dependencies spec-entry))
                                    ;; (version (spec-version spec-entry))
                                    )
                               (nix-expression egg native-deps)))
                           eggs)))
    (with-output-to-string
      (lambda ()
        (print "{ pkgs, stdenv }:")
        (print "rec {")
        (print "inherit (pkgs) eggDerivation fetchegg;")
        (map print expressions)
        (print "}\n")
        (delete-directory (temp-directory) #t)))))

(print (nix-file (car (read-file "./chicken-deps.scm"))))


