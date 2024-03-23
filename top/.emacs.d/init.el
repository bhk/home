;; init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printf (&rest a)
  (princ (concat (apply 'format a)) "\n"))

(defmacro filter-it (frm lst)
  "Return all members of LST for which FRM is non-nil.  In FRM, `it` will
be bound t the current item."
  (let ((result (make-symbol "result")))
    `(let (,result)
       (dolist (it ,lst)
         (if ,frm
             (push it ,result)))
       (reverse ,result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode 0)
(blink-cursor-mode 0)

(when (>= emacs-major-version 22)
  (set-variable 'inhibit-splash-screen t))

(defun my-zoom-frame (up-down))

(when (fboundp 'window-system)

  ;; frame properties
  (add-to-list 'default-frame-alist '(width  . 80))
  (add-to-list 'default-frame-alist '(height . 47))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))

  ;; my-zoom-frame
  (defvar my-default-size 14)
  (defvar my-size my-default-size)

  (defun my-zoom-frame (up-down)
    (setq my-size (if (= 0 up-down)
                      my-default-size
                    (+ my-size up-down)))
    ;; t => preserve window size
    (set-frame-font (format "%d" my-size) t)
    (message "Size: %d" my-size))

  (defun dark ()
    (interactive)
    (customize-set-variable 'custom-enabled-themes '(dark)))

  (defun light ()
    (interactive)
    (customize-set-variable 'custom-enabled-themes '(light))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dump environment info
(defun dump-env ()
  "Dump misc. environment information"
  (interactive)
  (dolist (e '(window-system (length (defined-colors)) process-environment
               system-configuration (version)))
       (insert (format "\n%s: %S" e (eval e)))))

;; Set a different server file name for each emacs instance, so two
;; different emacs instances will not clobber each other, and running
;; emacsclient in a sub-shells will invoke the correct instance.
(require `server)
(set-variable 'server-name (number-to-string (emacs-pid)))
(server-start)
(setenv "EDITOR" (concat "emacsclient -s " server-name))

;; Make C-x k terminate the buffer's client session (not just C-x #)
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

;; Note: The TERM variable is clobbered when sub-commands are invoked.  We
;; could use .emacs.d/init_bash.sh, but that causes multiple prompts to be
;; echoed into the buffer.  Instead, we rely on .bashrc to check
;; $INSIDE_EMACS in interactive shells.

(when (fboundp 'window-system)
  ;; In Mac/Win GUI emacs, the environment has not been initialized by a
  ;; login shell, so we need to ensure that .bash_profile and/or .bashrc
  ;; will be run when a subcommand is run via `M-x compile`.
  (setenv "BASH_ENV" (concat (getenv "HOME") "/.bash_profile"))

  ;; ...and for the same reason, ~/local/bin is not in exec-path, which will
  ;; cause (executable-find) not to find `aspell`.  This unavoidably
  ;; duplicates logic in .bashrc.
  (let ((localbin (concat (getenv "HOME") "/local/bin")))
    (add-to-list 'exec-path localbin))

  ;; Hunspell from MacPorts lives here...
  (add-to-list 'exec-path "/usr/local/bin")

  ;; If running a non-installed MacOS emacs, ensure that its bin directories
  ;; come first in the PATH, so subshells will get the appropriate
  ;; emacsclient.
  (if (string-match-p "/MacOS/libexec/$" exec-directory)
      (let* ((top (replace-regexp-in-string "/libexec/.*" "/" exec-directory))
             (re (concat (regexp-quote top) ".*"))
             (path (split-string (getenv "PATH") ":" t))
             (new (combine-and-quote-strings
                   (append
                    (filter-it (string-match-p re it) exec-path)
                    (filter-it (not (string-match-p re it)) exec-path))
                   ":")))
        (setenv "PATH" new)))

  (if (equal "/" default-directory)
      (setq default-directory (getenv "HOME"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tree)
(require 'p4utils)

(defun single-up ()
  (interactive) (scroll-down 1))

(defun single-down ()
  (interactive) (scroll-up 1))

(defun dos()
  "Select DOS newlines (see \\='unix\\=')"
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos))

(defun unix()
  "Select UNIX newlines (see \\='dos\\=')"
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix))

;; Kill just the active frame, unless there is only one frame.
;;
(defun kill-frame ()
  (interactive)
  (if (cdr (frame-list))
      (delete-frame)
    (if (fboundp 'save-buffers-kill-terminal)
        (save-buffers-kill-terminal)
      (save-buffers-kill-emacs))))

;; How did I get by without this?  26-Oct-95 bhk
(defun other-window-prev ()
  "Select the previous window on this screen; same as (`other-window' -1), but
bound to an interactive function so it can be assigned to a key.
bhk 18-Jan-96"
  (interactive)
  (other-window -1))

;; Is buffer 'b' a buffer we want to browse with prev & next?  Buffers we
;; did not create intentionally usually begin with a space or "*" and a
;; capital letter.
;;
(defun is-browsable-buffer (b)
  (let ((case-fold-search nil))
    (not (string-match "^\\( \\|\\*[A-Z]\\)" (buffer-name b)))))

;; And buffer history navigation   (Emissary-like)
;;   bury-buffer == go back in history
;;   next-buffer == go forward in history
 (defun next-buffer ()
   "Select the \\='next\\=' buffer a la forward/backward (browser-like)
buffer navigation. bury-buffer and next-buffer act as \\='back\\=' and
\\='forward\\=', respectively.  bhk Dec-95"
  (interactive)
  (switch-to-buffer (car (filter-it (is-browsable-buffer it)
                                    (reverse (buffer-list))))))

(defun prev-buffer ()
  "Select the \\='previous\\=' buffer a la forward/backward
browser-like buffer navigation."
  (interactive)
  (let ((start (current-buffer)))
    ;; With an explicit arg, bury-buffer will not switch the current window.
    (bury-buffer start)
    (while (and (not (eq start (car (buffer-list))))
                (not (is-browsable-buffer (car (buffer-list)))))
      (bury-buffer (car (buffer-list)))))
  (switch-to-buffer (car (buffer-list))))

(defun grep-in (path prompt)
  (require 'compile)
  (compile (concat "grep -n " (read-string prompt) " " path)))

;; search C/C++ sources in current directory
(defun c-grep ()
  (interactive)
  (grep-in "*.c *.h *.cc *.cpp *.bid *.cif *.lua *.idl *.min" "C/C++ grep: "))

;; extern-c
(defun extern-c (beg end)
  "Surround region with #ifdef __cplusplus extern \"C\" block."
  (interactive "*r")
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (insert "#ifdef __cplusplus\n}\n#endif\n")
    (goto-char beg)
    (beginning-of-line)
    (insert "#ifdef __cplusplus\nextern \"C\" {\n#endif\n")))

(defun ifndef-included ()
  "Surround remainder of buffer with multiple-inclusion guard"
  (interactive)
  (let ((name
         (concat "__" (replace-regexp-in-string "\\." "_" (upcase (buffer-name))))))
    (save-excursion
      (insert "#ifndef " name "\n#define " name "\n\n")
      (goto-char (point-max))
      (insert "\n\n#endif // " name "\n"))))

;; Don't confuse this ph with the ph library distributed with emacs.
(defun ph-shell (nm)
  (interactive "sPH Name: ")
  (shell-command (concat "ph " nm)))

(defun interact (nlines)
  "Open Lisp Interaction window at bottom of frame. Argument = number of
lines for new window."
  (interactive "p")
  (split-window-vertically (- (if (> nlines 1) nlines 16)))
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (lisp-interaction-mode)
  (font-lock-mode t))

;; compilation-error-regexp-alist : Test with ./lisp/mycomp.txt
;;
;; We can add regexps to the alist, but debugging is tedious and confounded
;; by the tendency for the built-in regexps to override our customizations,
;; regardless of whether we add them to the head or tail of the alist.  (The
;; undesired ones match "FILE:LINE" as file name, versus just "FILE".)
;;
;; My current solution is to drastically restrict the set of regexps by
;; defining my own alist from scratch.
;;
;; Hard lesson: Emacs regexp syntax for alternatives handled matching
;; strangely, so it's pretty useless for this use case.  The un-matched
;; branches emit 'nil' captures.
;;
;;    (match-string "\\(?:\\(A\\)\\|\\(B\\)\\)" "B")
;;    (match-data)  -->  (0 1 nil nil 0 1)
;;

(defvar original-compilation-error-regexp-alist
  nil)

(eval-after-load `compile
  '(let* ((num       "\\([0-9]+\\)")
          (file      "\\([^\n :()]+\\)")
          (file-abs  "\\(/[^\n :()]+\\)")
          (file-url  (concat "\\(?:file://\\)" file-abs))
          (file-url? (concat "\\(?:file://\\)?" file-abs))  ;; or abs..

          (nums     (concat ":" num "\\(?::" num "\\)?"))
          (flc      (concat file ":" num ":" num))
          (flc?     (concat file ":" nums))
          (flc-url  (concat file-url? ":" num ":" num))

          (my-alist-alist
           `((terser ,(concat "^Parse error at " file ":" num "," num "\n") 1 2 3)
             (gnufix ,(concat "^In file included from " flc? ":\n") 1 2 3)
             (node-a ,(concat "^    at [A-Za-z_][^:/()\n]* (" flc-url ")\n") 1 2 3)
             (node-b ,(concat "^Error [^\n]+imported from " file "\n") 1)
             (node-c ,(concat "^\\(?:    at \\)?" file-url? nums ":?\n") 1 2 3)
             (rollup ,(concat "^\\[!\\] Error[^\n]*\n" file " (" num ":" num ")\n") 1 2 3)))

          (my-alist (mapcar 'car my-alist-alist)))

     ;; add custom symbol defs to alist^2
     (setq compilation-error-regexp-alist-alist
           (append my-alist-alist compilation-error-regexp-alist-alist))

     (setq compilation-error-regexp-alist
           (append my-alist '(gnu)))))

;; shell
(defun shell-bottom (arg)
  (interactive "p")
  (let ((suffix (if (eq arg 1)
                    default-directory
                  (number-to-string arg))))
    (shell (concat "*shell " suffix "*")))
  (goto-char (point-max)))

;;------------------------------
;; cwdtrack: like dirtrack, but not brain dead.  dirtrack hoses shell mode
;; badly if the prompt regexp matches something that isn't a directory.
;;------------------------------

(defvar cwdtrack-regexp
  ;; default:
  ;;   dir begins with ~, / or <alpha>:<slash>, ends at ], >, :, or %
  ;;   skip any preceding '[hostname]'
  ;;   directory may contain spaces (up to one trailing space is stripped)
  "\\(?:\\[[^ ]*\\] *\\)?\\(\\(?:[~/]\\|[a-zA-Z]:[/\\\\]\\)[^][\n]*?\\) ?[]:>\\%] ?$"
  "*Used by `cwdtrack-mode' to match directory names in prompts.
The first capture should return the matched directory.  The
expression should be prepared to match against multiple lines of
text, including text that precedes the actual prompt (shell-mode
gets into this state sometimes).  It should not match relative
paths, empty strings, or embedded newlines.  If a custom regexp
doesn't seem to work, use `cwdtrack-debugp'.")

;;;; Test cwdtrack-regexp
;
;(defun match1 (regex str)
;  (let ( (pos (string-match regex str)) )
;    (match-string 1 str)))
;(setq cwdtrack-regexp ...)  ;; re-eval of defvar not reliable?
;(match1 cwdtrack-regexp "~/src] ")
;(match1 cwdtrack-regexp "~] ")
;(match1 cwdtrack-regexp "[host] ~/src: ")

(defvar cwdtrack-debugp nil
  "*When non-nil, log the results of matching `cwdtrack-regexp'.")

(defvar cwdtrack-previous "" "Directory most recently matched")
(make-variable-buffer-local 'cwdtrack-previous)

(defun cwdtrack-filter (input)
  (let (dir)
    (and (string-match cwdtrack-regexp input)
         (setq dir (match-string 1 input))
         (not (string= cwdtrack-previous dir))
         (setq cwdtrack-previous dir)
         (file-accessible-directory-p dir)
         (fboundp 'shell-process-cd)  ;; avoids compiler warning
         (shell-process-cd dir))
    (if cwdtrack-debugp
        (message "cwdtrack: `%s' -> `%s'" input dir)))
  input)

(defun cwdtrack-mode (&optional arg)
  "*Enable/disable cwdtrack (disables when ARG equals 0).

cwdtrack updates a shell buffer's current working directory by
scaning process output for prompt strings containing directory
names.  Customize with `cwdtrack-regexp'."
  (interactive "p")
  (if (eq arg 0)
      (remove-hook 'comint-preoutput-filter-functions 'cwdtrack-filter t)
    (add-hook 'comint-preoutput-filter-functions 'cwdtrack-filter nil t))
  (if arg
      (message "cwdtrack mode is %sabled." (if (eq arg 0) "dis" "en"))))

(when (> emacs-major-version 21)
  (add-hook 'shell-mode-hook 'cwdtrack-mode)
  ;; turn off default tracking
  (add-hook 'shell-mode-hook 'shell-dirtrack-mode))

;;------------------------------
;; Construct review comments
;;------------------------------

(defvar review-buffer nil)

(defun review-add-comment ()
  (interactive)
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (dummy (or (and review-buffer
                         (buffer-live-p review-buffer))
                    (setq review-buffer
                          (find-file-noselect
                           (read-file-name "File for review comments: "
                                           (file-name-directory file)
                                           "review.txt")))))
         (review-file (buffer-file-name review-buffer))
         (rfile (file-relative-name file
                                    (file-name-directory review-file))))

    (switch-to-buffer-other-window review-buffer)
    (goto-char (point-max))
    (insert (format "\n%s:%d: " rfile line))))

;;------------------------------
;; save-and-make: faster, better, cheaper
;;------------------------------

(defvar save-and-make-command "make")
(defun save-and-make (arg)
  (interactive "P")
  (if arg
      (setq save-and-make-command (read-string "Make command: " save-and-make-command)))
  (let ((compilation-ask-about-save nil))
    (compile save-and-make-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes & Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq transient-mark-mode t)
(setq truncate-partial-width-windows nil)
(setq search-highlight t)
(delete-selection-mode t)
(setq scroll-step 1)
(put 'eval-expression 'disabled nil)    ; M-:

(require 'font-lock)
(global-font-lock-mode t)

(autoload 'rust-mode "rust-mode" nil t)
(autoload 'lua-mode "lua-mode.el" nil t)
(autoload 'ruby-mode "ruby-mode.el" nil t)

(setq auto-mode-alist
      (append
       '(("\\.8\\'" . asm-mode)
         ("\\.cjs\\'" . javascript-mode)
         ("\\.pl\\'" . prolog-mode)
         ("\\.rs\\'" . rust-mode)
         ("\\.[ch]\\'" . c++-mode)
         ("\\.bid\\'" . c++-mode)
         ("\\.min\\'" . makefile-gmake-mode)
         ("\\.mak\\'" . makefile-gmake-mode)
         ("\\.mk\\'" . makefile-gmake-mode)
         ("[Mm]akefile\\'" . makefile-gmake-mode)
         ("scam\\'" . makefile-gmake-mode)
         ("SCons[criptu]+t\\'" . python-mode)
         ("Package\\'" . makefile-gmake-mode)
         ("^\\.config\\'" . makefile-gmake-mode)
         ("\\.lua\\'" . lua-mode)
         ("^pak\\'" . lua-mode)
         ("\\.rb\\'" . ruby-mode))
       auto-mode-alist))

;; Spell checking

(eval-when-compile
  (require 'ispell)) ;; avoid "free variable" warnings

(cond
 ((executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (defvar ispell-really-hunspell t))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-dictionary "english"
        ispell-personal-dictionary "/Users/bkelley/.aspell.pws"
        ispell-extra-args (list (concat "--home-dir=" (expand-file-name "~/"))))))

;; "GDB mode"

(defun my-gud-mode-hook ()
  (global-set-key [?\C-c ?\C-,] 'gud-up)
  (global-set-key [?\C-c ?\C-.] 'gud-down))

(add-hook 'gud-mode-hook 'my-gud-mode-hook)

;; Disable insecure shell mode behavior.
(with-no-warnings
  (setq tramp-mode nil))

(defun my-lua-mode-hook ()
  (local-set-key "\C-cl" 'lua-send-region))
(add-hook 'lua-mode-hook 'my-lua-mode-hook)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'accept-process-output)

;; Delete trailing whitespace from lines before a file is saved.
;; (unless we're editing a makefile)
(defun clean-whitespace ()
  (unless (string-match "^[Mm]ake" (symbol-name major-mode))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'clean-whitespace)

;; C-mode

(eval-when-compile (require 'cc-mode))
(defun my-c-mode-common-hook ()
  (setq tab-width 8
        indent-tabs-mode nil        ; Use spaces instead of tabs
        c-basic-offset 3)           ; mother of all indentation parameters
  (font-lock-mode 1)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; asm-mode

(defun my-asm-mode-hook ()
  (setq indent-tabs-mode t)
  (define-key (current-local-map) "\M-;" 'asm-comment)
  (define-key (current-local-map) [?\;] 'self-insert-command))

(add-hook 'asm-mode-hook 'my-asm-mode-hook)


(defun my-limode-hook ()
  (local-set-key [C-return] 'eval-last-sexp))
(add-hook 'lisp-interaction-mode-hook 'my-limode-hook)


(defun my-artist-mode-hook ()
  (when (boundp 'artist-mode-map)
    ;; button 3 = right button on two-button mouse
    (define-key artist-mode-map [down-mouse-3] 'artist-mouse-choose-operation)))

(add-hook 'artist-mode-init-hook 'my-artist-mode-hook)

(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))

(defun find-log ()
  (interactive)
  (find-file "~/Dropbox/txt/log.txt"))

(defun find-todo ()
  (interactive)
  (find-file "~/Dropbox/txt/todo.txt"))

;; use IBM PC character set for *.8 files
(modify-coding-system-alist 'file "\\.8\\'" 'cp437)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Revealing cryptic emacs key binding syntax:
;;   1. Use global-set-key to bind a key
;;   2. repeat-complex-command (C-x ESC eSC)

(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)
(define-key emacs-lisp-mode-map [C-return] 'eval-last-sexp)

(global-set-key "\M-p" 'single-up)
(global-set-key "\M-n" 'single-down)
(global-set-key [end]  'end-of-line)
(global-set-key [home] 'beginning-of-line)
(global-set-key [M-left] 'prev-buffer)        ; '(meta left)
(global-set-key [M-right] 'next-buffer)
(global-set-key [M-up] 'other-window-prev)
(global-set-key [M-down] 'other-window)
(global-set-key [27 left] 'prev-buffer)     ; Alt-Left in MacOS Terminal
(global-set-key [27 right] 'next-buffer)
(global-set-key [27 up] 'other-window-prev)
(global-set-key [27 down] 'other-window)
(global-set-key [f9]  'next-error)
(global-set-key [f10] 'previous-error)
(global-set-key [f11] 'grep)
(global-set-key [f12] 'c-grep)
(global-set-key "\C-x\C-j" 'tree-hyperjump)
(global-set-key "\C-xj" 'goto-line)
(global-set-key "\C-x?" 'jump-to-register)
(global-set-key "\C-x\C-g" 'grep)
(global-set-key "\C-x\C-c" 'kill-frame)
(global-set-key [f1] 'find-tag)
(global-set-key [f2] 'tags-search)
(global-set-key [f3] 'tags-loop-continue)
(global-set-key [f7] 'compile)
(when (>= emacs-major-version 23)
  (global-set-key [?\C-=] 'text-scale-increase)
  (global-set-key [?\C--] 'text-scale-decrease))

;; Use C-z for private keymap (more portable than F-keys)
;;
(global-set-key "\C-z" nil)  ;; suspend-emacs not that useful
(global-set-key "\C-z\C-a" 'review-add-comment)
(global-set-key "\C-z\C-f" 'tree-find-file)
(global-set-key "\C-z\C-g" 'tree-grep)
(global-set-key "\C-z\C-h" 'tree-grep-headers)
(global-set-key "\C-z\C-j" 'tree-hyperjump)
(global-set-key "\C-z\C-m" 'save-and-make)
(global-set-key "\C-z\C-n" 'next-error)
(global-set-key "\C-z\C-o" 'p4-offline)
(global-set-key "\C-z\C-p" 'previous-error)
(global-set-key "\C-z\C-s" 'shell-bottom)
(global-set-key "\C-z\C-t" 'ansi-term)
(global-set-key "\C-zg"    'c-grep)
(global-set-key "\C-zm"    'compile)
(global-set-key "\C-z\C-\\" 'toggle-truncate-lines)
(global-set-key "\C-z\C-z" 'find-log)
(global-set-key "\C-zz" 'find-todo)
;; Horizontal scrolling (see toggle-truncate-lines)
(global-set-key [?\C-,] (lambda () (interactive) (scroll-left 4)))
(global-set-key [?\C-.] (lambda () (interactive) (scroll-right 4)))
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] 'other-window-prev)
(global-set-key [mouse-3] 'tree-mouse-hyperjump)
(global-unset-key "\C-xnn") ; narrow-to-region

;; Historical backspace problems...
;; (global-set-key "\C-h" 'backward-delete-char))
;; (global-set-key [deletechar] 'backward-delete-char)

;; Adjust font size for entire frame
(global-set-key "\M-=" (lambda () (interactive) (my-zoom-frame 1)))
(global-set-key "\M--" (lambda () (interactive) (my-zoom-frame -1)))
(global-set-key "\M-0" (lambda () (interactive) (my-zoom-frame 0)))

;; SCAM syntax
(put 'let-global 'scheme-indent-function 1)
(put 'let& 'scheme-indent-function 1)
(put 'data 'scheme-indent-function 1)
(put 'for 'scheme-indent-function 1)
(put 'concat-for 'scheme-indent-function 1)
(put 'append-for 'scheme-indent-function 1)
(put 'foreach 'scheme-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-host
  (or (getenv "COMPUTERNAME") (system-name)))
(if my-host
    (load (concat "~/.emacs-" my-host) t t))
