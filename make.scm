(require "core")

;;----------------  Configuration  ----------------

;; directory containing all files to be deployed
(define top "top")

;; directories under `top` to be deployed as a directory symlink
(define linked-dirs ".emacs.d .config/git")


;;----------------  Utilities  ----------------

;; vec-relpath works on vectors of path elements
;;
(define (vec-relpath from to)
  (if (and from
           (eq? (first from) (first to)))
      (vec-relpath (rest from) (rest to))
      (append (for (x from) "..")
              to)))

;; Return relative path from directory `from` to file/directory `to`.
;;
(define (relpath from to)
  (define `(elements path)
    (split "/" (abspath path)))
  (concat-vec (vec-relpath (elements from) (elements to))
              "/"))

;; Return all files in `names` and all files underneath directories in `names`.
;;
(define (find-files names)
  (foreach (name names)
    (let ((members (wildcard (concat name "/.* " name "/*"))))
     (if members
         (find-files (filter-out "%/.. %/." members))  ;; is directory
         name))))

(define `(exists? path)
  (wildcard path))

(define (is-symlink? path)
  (shell (concat "if [ -L '" path "' ] ; then echo 1 ; fi")))

;;----------------  Install/Uninstall  ----------------

(define help-raw
  "Usage:
   make help         Display this message
   make install      Install symbolic links
   make uninstall    Remove symbolic links

The `top` directory contains an image of files to be propagated to your
$HOME directory.

Instead of copying the files, `make install` creates symbolic links so that
this repository can be easily updated using git.  `make install` avoids
removing files or links that already exist; you may have to manually remove
some files in order to deploy the ones in this project.  Also, you can
manually replace a symbolic link with a copy in order to maintain the local
$HOME directory in a state that diverges from the project (e.g. work
machines vs. personal machines).

In some cases, symbolic links are created to directories rather than
individual files.  This can make it easier to track files that are added to
those directories and propagate changes back into this project.  Linked
directories are: LDIRS

Modify the `linked-dirs` variable in make.scm to change this.
")

(define `help-str
  (subst "LDIRS" (foreach (d linked-dirs) (subst "D" d "\n   ~/D/"))
         help-raw))

;; Create a symbolic link from dest to rel
;;
(define (install-file dest target)
  (if (not (exists? (dir dest)))
      (begin
        (print "Creating directory: " (dir dest))
        (shell (concat "mkdir -p " (dir dest)))))

  (if (exists? dest)
      (if (is-symlink? dest)
          (print "Skipping " target " (already a symlink)")
          (print "AVOIDING " target " (remove pre-existing file first)"))
      (begin
        (shell (concat "ln -fs " (relpath (dir dest) top) "/" target " " dest))
        (print "Installed " target))))


;; Remove a previously-created symbolic link
;;
(define (uninstall-file dest)
  (if (is-symlink? dest)
      (begin
        (print "Removing " dest)
        (shell (concat "rm " dest)))
      (if (exists? dest)
          (print "LEAVING " dest " (not a symlink)")
          (print "Missing " dest))))

(define (visit-files action)
  (define `(not-under dirs files)
    (filter-out (addsuffix "/%" dirs) files))

  (define `files (find-files top))
  (define `tails (patsubst (concat top "/%") "%" files))

  ;; symbolic links to be created (relative to HOME)
  (define `links (append (not-under linked-dirs tails)
                         linked-dirs))

  (foreach (f links)
    (action (concat (native-var "HOME") "/" f) f)))


(define rules "help show install uninstall")

(define (build rule)
  (cond ((eq? rule "help") (print help-str))
        ((eq? rule "show") (visit-files (lambda (a b) (print a " --> " b))))
        ((eq? rule "install") (visit-files install-file))
        ((eq? rule "uninstall") (visit-files uninstall-file))))


(define (main args)
  ;; Create a phony rule for each name in `rules`

  (define `phony-rule
    (concat ".PHONY: X\n"
            "X: ; @true " (lambda () (build "X")) "\n"))

  (foreach (r rules)
    (native-eval (subst "X" r phony-rule)))

  ;; Rebuild makefile from this source file when necessary
  (native-eval "Makefile: make.scm; @top/local/bin/scam -o $@ $<"))
