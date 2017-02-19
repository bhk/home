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

(push (concat edir "/lisp") load-path)  ;; needed by byte-compile-file
(mapcar 'freshen-elc (directory-files edir t "elc$"))
(freshen-elc (concat edir "/init.elc"))

(load (concat edir "/init"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dark)))
 '(custom-safe-themes
   (quote
    ("a95fc0516c80b09f50d4f34179185b92c8368f8ed3b2089b21a0dc1ab3f13392" "05225f9a0dd77096dc2bde4ad721ea4a901fde0a9a00afeba7dc874ca1413396" "8e07bb364a54330ace86aa39c991abbbb31c16c2f865bdc5ae4c872fa1ae8acc" "b19ec98ce8db0e3a9e2660be0b9d4eeb335173fbf4bfc864824edeec327eacf5" default)))
 '(fill-column 76)
 '(indent-tabs-mode nil)
 '(ps-landscape-mode t)
 '(ps-line-number t)
 '(ps-number-of-columns 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Source Code Pro")))))
