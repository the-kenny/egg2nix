(use posix)
(use extras)
(use srfi-1)
(use matchable)
(use http-client)
(use versions)

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
          (print "loading versions...")
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

(define (local-egg-path egg)
  (let ((x (string-append (temp-directory) (egg-name egg)
                          "-"
                          (or (egg-version egg) (latest-version egg))
                          "/")))
    (print (format "local egg path (~A): ~A" egg x))
    x))

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
        (assert (member version (all-versions name))))
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
    (append (cdr (or (assq 'depends meta) '(())))
            (cdr (or (assq 'needs meta) '(()))))))


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
  (remove (cut egg-with-higher-version-in-list? <> eggs) eggs))

(define (all-dependencies-internal egg acc)
  (let* ((name (egg-name egg))
         (version (egg-version egg)))
    ;; Don't collect deps if `name' is already in `acc'
    (if (egg-in-list? egg acc)
        acc
        (begin
          (print (string-append "collecting deps for " name " (" (or version "latest") ")"))
          ;; Make sure egg `name' is downloaded
          (if (egg-with-higher-version-in-list? egg acc)
              acc
              (begin
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
                               (all-dependencies-internal egg acc))
                             acc
                             dependencies)))))))))

(define (all-dependencies egg)
  (let ((name (egg-name egg)))
    (remove (lambda (egg) (equal? (egg-name egg) name))
            (all-dependencies-internal egg '()))))

(define (highest-version egg-name)
  (first (version-sort (all-versions egg-name) #f)))

(define (nix-deps-string deps)
  (string-append
   "[\n    "
   (string-join (map egg-name deps)
                "\n    ")
   "\n  ]"))

(define (nix-expression egg)
  (print (format "nix-expression for ~A" egg))
  (let* ((name (egg-name egg))
         (version (or (egg-version egg)
                      (highest-version name)))
         (hash (nix-hash egg))
         (deps (remove-older-duplicate-eggs (all-dependencies egg))))
    (format
     "
~A = eggDerivation {
  name = \"~A-~A\";

  src = fetchegg {
    name = \"~A\";
    version = \"~A\";
    sha256 = \"~A\";
  };

  chickenDeps = ~A;
};
"
     name
     name
     version
     name
     version
     hash
     (nix-deps-string deps))))

(define (collect-eggs spec)
  (let* ((eggs spec)
         (deps (map (lambda (egg)
                      (all-dependencies egg))
                    eggs))
         (all (foldl append eggs deps)))
    (remove-older-duplicate-eggs all)))

(define (nix-file spec)
  (let* ((eggs (collect-eggs spec))
         (expressions (map nix-expression eggs)))
    (with-output-to-string
      (lambda ()
        (print "{ stdenv, chicken, fetchegg, pkgs, eggDerivation }:")
        (print "rec {")
        (map print expressions)
        (print "}\n")))))
