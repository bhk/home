;; dir of elisp files
(defvar edir "~/.emacs.d")

;; re-compile stale ELC files
(defun freshen-elc (elc)
  (let ((el (replace-regexp-in-string "c$" "" elc t t)))
    (and (file-exists-p el)
         (file-newer-than-file-p el elc)
         (if (byte-compile-file el)
             (print (concat "Compiled " elc))
           (error "Failed compiling %s" elc)))))

(push edir load-path)  ;; needed by byte-compile-file
(mapcar 'freshen-elc (directory-files edir t "elc$"))

(load (concat edir "/init"))

