(use posix)
(use extras)
(use srfi-1)
(use matchable)
(use http-client)
(use versions)

(define (temp-directory)
  (let ((dir "/tmp/egg2nix/"))
    (if (not (directory? dir))
        (create-directory dir #t))
    dir))

(define (local-egg-path name)
  (string-append (temp-directory) name "/"))

(define (chicken-install-retrieve name #!optional version)
  (process "chicken-install"
           (list "-retrieve"
                 (if version
                     (string-append name ":" version)
                     name))))

(define (nix-hash name)
  (assert (directory? (local-egg-path name)))
  (let-values (((in out pid) (process "nix-hash"
                                      (list "--base32"
                                            "--type"
                                            "sha256"
                                            (local-egg-path name)))))
    (string-trim-right (read-string #f in))))

(define known-versions '())

(define (all-versions egg-name)
  (let ((known (assoc egg-name known-versions)))
    (if known
        (cdr known)
        (begin
          (print "loading versions...")
          (with-input-from-request
           (string-append "http://code.call-cc.org/cgi-bin/henrietta.cgi?name=" egg-name "&listversions=1")
           #f
           (lambda ()
             (let ((versions (string-split (read-string) "\n")))
               (set! known-versions (cons (cons egg-name versions) known-versions))
               versions)))))))

(define (retrieve-egg name #!optional version)
  (change-directory (temp-directory))
  ;; Check if `version' is a valid release.
  (if version
      (assert (member version (all-versions name))))
  (let ((path (local-egg-path name)))
    (if (not (directory? path))
        (let*-values (((in out pid) (chicken-install-retrieve name version))
                      ((pid normally status) (process-wait pid)))
          (if (= 0 status)
              path))
        path)))

(define (egg-meta name)
  (car
   (read-file (string-append (local-egg-path name)
                             name ".meta"))))

(define (egg-dependencies meta)
  (append (cdr (or (assq 'depends meta) '(())))
          (cdr (or (assq 'needs meta) '(())))))


(define (egg-in-list? egg-name lst)
  (any (lambda (x) (string= egg-name
                            (match x
                              ((name version)
                               name)
                              (name
                               name))))
       lst))

(define (egg-name egg)
  (if (list? egg)
      (first egg)
      egg))

(define (egg-version egg)
  (if (list? egg)
      (second egg)
      #f))

(define (all-dependencies-internal name #!optional version acc)
  (let* ((name (if (symbol? name) (symbol->string name) name))
         (version (if (number? version) (number->string version) version))
         (name:version (if version (list name version) name)))
    ;; Don't collect deps if `name' is already in `acc'
    (if (egg-in-list? name acc)
        acc
        (begin
          (print (string-append "collecting deps for " name))
          ;; Make sure egg `name' is downloaded
          (retrieve-egg name version)
          (let ((dependencies (egg-dependencies (egg-meta name)))
                ;; Add egg `name' to the accumulated list of deps
                (acc (cons name:version acc)))
            ;; Leaf of the dep-tree
            (if (not dependencies)
                acc
                ;; Add all dependencies by folding over them. This
                ;; assures we don't load a dependency twice.
                ;;
                ;; NOTE: We *always* load the latest release of a
                ;; dependency. This is BY-DESIGN.
                (foldl (lambda (acc egg)
                         (all-dependencies-internal (egg-name egg) #f acc))
                       acc
                       dependencies)))))))

(define (all-dependencies name #!optional version)
  (remove (lambda (egg) (equal? (egg-name egg) name))
          (all-dependencies-internal name version '())))

(define (highest-version egg-name)
  (first (version-sort (all-versions egg-name) #f)))

(define (nix-deps-string deps)
  (string-append
   "[\n    "
   (string-join (map egg-name deps)
                "\n    ")
   "\n  ]"))

(define (nix-expression egg)
  (let* ((name (egg-name egg))
         (version (or (egg-version egg)
                      (highest-version name)))
         (hash (nix-hash name))
         (deps (all-dependencies name version)))
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

(define (nix-file spec)
  (let* ((eggs spec)
         (deps (map (lambda (egg)
                      (all-dependencies (egg-name egg) (egg-version egg)))
                    eggs))
         (eggs (delete-duplicates (foldl append eggs deps)))
         (expressions (map nix-expression eggs)))
   (with-output-to-string
     (lambda ()
       (print "{ stdenv, chicken, fetchegg, pkgs, eggDerivation }:")
       (print "rec {")
       (map print expressions)
       (print "}\n")))))
