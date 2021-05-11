(import (test))
(import (chicken file))
(import (chicken load))
(import (chicken port))
(import (chicken pathname))
(import (chicken process-context))

(load-relative "../egg2nix.scm")

(import (egg2nix))

(test-begin "egg2nix")

(define (with-clean-directory thunk)
  (let ((pwd (current-directory))
        (dir (create-temporary-directory)))
    (dynamic-wind
     (lambda ()
       (set! (current-directory) dir))
     (lambda () (thunk))
     (lambda ()
       (set! (current-directory) pwd)
       (delete-directory dir #t)))))

(test-group "egg file location"
  (with-clean-directory
   (lambda ()
     (with-output-to-file "foo.egg" void)
     (test "with a local egg file"
           (make-pathname (current-directory) "foo.egg")
           (locate-egg-file))))
  (with-clean-directory
   (lambda ()
     (create-directory "chicken")
     (with-output-to-file "chicken/foo.egg" void)
     (test "with an egg file in the chicken directory"
           (make-pathname (current-directory) "chicken/foo.egg")
           (locate-egg-file))))
  (with-clean-directory
   (lambda ()
     (test-error "when no egg file is found" (locate-egg-file))))
  (with-clean-directory
   (lambda ()
     (with-output-to-file "foo.egg" void)
     (with-output-to-file "bar.egg" void)
     (test-error "when multiple egg files are found" (locate-egg-file)))))

(define (deps str)
  (with-input-from-string str read-dependencies))

(test-group "input handling"
  (test-group "dependency list"
    (test "with no dependencies" '() (deps ""))
    (test "with a single dependency" '(srfi-1) (deps "srfi-1"))
    (test "with multiple dependencies" '(srfi-1 args) (deps "srfi-1\nargs"))
    (test "with a dependency version" '((srfi-1 "1.0")) (deps "(srfi-1 \"1.0\")")))
  (test-group "egg files"
    (test "with no dependencies" '() (deps "((components))"))
    (test "with a single dependency" '(srfi-1) (deps "((components) (dependencies srfi-1))"))
    (test "with multiple dependencies" '(srfi-1 args) (deps "((components) (dependencies srfi-1 args))"))
    (test "with a dependency version" '((srfi-1 "1.0")) (deps "((components) (dependencies (srfi-1 \"1.0\")))"))))

(test-end "egg2nix")

(test-exit)
