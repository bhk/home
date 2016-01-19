(require "core")

;;----------------  Configuration  ----------------

;; directory containing all files to be deployed
(define top "top")

;; directories under `top` to be deployed as a directory symlink
(define linked-dirs ".emacs.d .config")


;;----------------  Utilities  ----------------

;; vec-relpath works on vectors of path elements
;;
(define (vec-relpath from to)
  (if (and from
           (eq (first from) (first to)))
      (vec-relpath (rest from) (rest to))
      (append (for x from "..")
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
  (foreach
   name names
   (let ((members (wildcard (concat name "/.* " name "/*"))))
     (if members
         (find-files (filter-out "%/.. %/." members))  ;; is directory
         name))))

(define `(exists? path)
  (wildcard path))

(define (is-symlink? path)
  (shell (concat "if [ -L '" path "' ] ; then echo 1 ; fi")))

;;----------------  Install/Uninstall  ----------------

(define help-str
  "Usage:
   make help         Display this message
   make install      Install symbolic links
   make uninstall    Remove symbolic links")

;; Create a symbolic link from dest to rel
;;
(define (install-file dest target)
  (if (not (exists? (dir dest)))
      (begin
        (print "Creating directory: " (dir dest))
        (shell (concat "mkdir -p " (dir dest)))))

  (if (and (exists? dest)
           (not (is-symlink? dest)))
      (print "AVOIDING " target " (remove pre-existing file first)")
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
  (declare HOME)

  (define `(not-under dirs files)
    (filter-out (addsuffix "/%" dirs) files))

  (define `files (find-files top))
  (define `tails (patsubst (concat top "/%") "%" files))

  ;; symbolic links to be created (relative to HOME)
  (define `links (append (not-under linked-dirs tails)
                         linked-dirs))

  (foreach f links
           (action (concat HOME "/" f) f)))


(define rules "help install uninstall")

(define (build rule)
  (cond ((eq rule "help") (print help-str))
        ((eq rule "install") (visit-files install-file))
        ((eq rule "uninstall") (visit-files uninstall-file))))


(define (main args)
  ;; Create a phony rule for each name in `rules`

  (define `phony-rule
    (concat ".PHONY: X\n"
            "X: ; @true " (lambda () (build "X")) "\n"))

  (foreach r rules
           (eval (subst "X" r phony-rule)))

  ;; Rebuild makefile from this source file when necessary
  (eval "Makefile: make.scm; @top/local/bin/scam -o $@ $< && rm *.min"))
