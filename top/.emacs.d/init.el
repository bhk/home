;; init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  Appearance settings  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-host (or (getenv "COMPUTERNAME") (system-name)))

(eval-when-compile (require 'cl))

(menu-bar-mode 0)
(blink-cursor-mode 0)

(when (>= emacs-major-version 22)
  (set-variable 'inhibit-splash-screen t))

;; Use "emacs -f bigfont" for large font; harmless when not supported.
(defun bigfont() )
(defun change-font( &optional key) )

(when (fboundp 'window-system)

  (if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))

  (if (fboundp 'w32-select-font)
      (defun select-font ()
        (interactive)
        (set-default-font (w32-select-font))))

  (defvar dflt-fonts
        '(("6x13"    "-*-6x13-normal-r-*-*-13-97-*-*-c-*-*-ansi-" 16)
          ("6x11"    "-*-6x11-normal-r-*-*-11-97-*-*-c-*-*-ansi-" 14)
          ("term8"   "-*-Terminal-normal-r-*-*-11-100-*-*-c-*-*-*-" 8)
          ("term9"   "-*-Terminal-normal-r-*-*-12-90-*-*-c-*-*-*-" 12)
          ("luc8"    "-*-Lucida Console-normal-r-*-*-11-82-*-*-c-*-*-ansi-" 11)
          ("luc9"    "-*-Lucida Console-normal-r-*-*-12-90-*-*-c-*-*-ansi-" 12)
          ("luc6n"   "-*-Lucida Console-medium-r-*-*-9-*-*-*-C-65-IS08859-" 9)
          ("luc7n"   "-*-Lucida Console-medium-r-*-*-10-*-*-*-C-65-IS08859-" 10)
          ("luc8n"   "-*-Lucida Console-medium-r-*-*-11-82-*-*-C-60-ISO8859-" 11)
          ("luc10"   "-*-Lucida Console-normal-r-*-*-13-97-*-*-c-*-*-ansi-" 13)
          ("luc12"   "-*-Lucida Console-normal-r-*-*-15-97-*-*-c-*-*-ansi-" 15)
          ("deja18"  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
          ("deja15"  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
          ("deja13"  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
          ("deja11"  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
          ("src13"   "-apple-Source_Code_Pro-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
          ("src18"   "-apple-Source_Code_Pro-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
          ("hack13"  "-*-Hack-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
          ("hack18"  "-*-Hack-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")
          ("menlo13" "-apple-Menlo-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
          ("and12"   "-*-Andale Mono-*" 15)))

  (defun change-font (&optional key)
    "Select font by short name, set default font, and preserve frame size."
    (interactive);; "sFont name: ")
    (let ((pixel-hgt (* (frame-height) (frame-char-height))))
      (if (null key)
	  (setq key (completing-read "Font: " dflt-fonts nil t)))
      ;; Font height, as of 20.6 (?):  (elt (font-info FONTNAME) 4)
      (let ((scm (assoc key dflt-fonts)))
	(when scm
	  (set-default-font (elt scm 1))
	  (set-frame-height nil (round (/ (float pixel-hgt) (frame-char-height))))))))

  ;; Choose default based on computer name
  (let ((font-assoc (assoc my-host
			   '(("SEDITION" "luc9")
			     ("BKELLEY1" "luc7n")
			     ("u64" "deja11")
			     ("BKELLEY" "luc8")))))
    (if font-assoc (change-font (cadr font-assoc))))

  ; (set-screen-width 118)

  (defun bigfont ()
    (change-font "luc12")))


;; X font names:
;;
;; -foundry-family-weight-slant-setwidth-addstyle-pxsize-ptsize-resx-resy-spacing-avgwid-charset-encoding
;;
;;   weight = medium | bold | ...
;;   slant = R | I | O | ...
;;   setwidth = Normal | Condensed | Narrow | Double Wide
;;   addstyle = Serif | Sans Serif | Informal | Decorated
;;   pxsize = pixel size, incorporating vertial spacing part of font design
;;   spacing = P | M | C   [proportional, monospaced, character cell]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;       Font-lock       ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tip: "M-x list-faces-display <RET>"
(require 'font-lock)

(with-no-warnings
  ;; security concerns re: shell mode behavior
  (setq tramp-mode nil))

(defun turn-on-font-lock ()
  (font-lock-mode 1))

(defun my-lua-mode-hook ()
  (local-set-key "\C-cl" 'lua-send-region))

(add-hook 'lua-mode-hook      'turn-on-font-lock)
(add-hook 'makefile-mode-hook 'turn-on-font-lock)
(add-hook 'lisp-mode-hook     'turn-on-font-lock)
(add-hook 'perl-mode-hook     'turn-on-font-lock)
(add-hook 'tex-mode-hook      'turn-on-font-lock)
(add-hook 'texinfo-mode-hook  'turn-on-font-lock)
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)

(add-hook 'shell-mode-hook    'accept-process-output)
(add-hook 'lua-mode-hook      'my-lua-mode-hook)

(defun deface (sym fg bg)
  (set-face-foreground sym fg)
  (set-face-background sym bg))

(defun colorscheme-green-on-black ()
  "Choose green-on-black color scheme."
  (interactive)
  (set-background-color    "black")
  (set-foreground-color    "#ffff60")
  (set-foreground-color    "#80ff60")
  (set-cursor-color        "yellow")

  (deface 'font-lock-variable-name-face "#ffff60"         "black")
  (deface 'font-lock-function-name-face "white"           "black")
  (deface 'font-lock-comment-face       "#c0c080"         "black")
  (deface 'font-lock-string-face        "turquoise1"      "black")
  (deface 'font-lock-keyword-face       "light goldenrod" "black")
  (deface 'font-lock-type-face          "gray"           "black")

  (set-face-background 'region "grey25"))

(defun colorscheme-wheat ()
  ;; Default colors
  (interactive)
  (set-background-color    "#f8ffec")
  (set-foreground-color    "black")
  (set-cursor-color        "#8080a0")

  (when (boundp 'default-frame-alist)
    (add-to-list 'default-frame-alist '(background-color . "#f8ffec")))

  (if (>= emacs-major-version 22)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(font-lock-builtin-face ((((class color) (min-colors 88) (background light)) (:foreground "red4"))))
     '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "CadetBlue4"))))
     '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "blue3"))))
     '(font-lock-keyword-face ((((class color) (min-colors 88) (background light)) (:foreground "green4"))))
     '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "burlywood4"))))
     ;; makefile-mode uses variable-name-face a lot

     '(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "SpringGreen4")))))))

;; Color scheme notes
;;
;; 'xterm-color' (Mac terminal) has a limited palette; nearest match can be ugly.
;;

(eval-when-compile
  (require 'ispell)) ;; avoid "free variable" warnings

(eval-after-load 'ispell
  (setq ispell-program-name "aspell"
        ispell-dictionary "english"
        ispell-personal-dictionary "/Users/bkelley/.aspell.pws"
        ispell-extra-args (list (concat "--home-dir=" (expand-file-name "~/")))
        ispell-dictionary-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;   Misc. Utilities   ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printf (&rest a)
  (princ (concat (apply 'format a)) "\n"))

(require 'tree)
(require 'p4utils)
(autoload 'lua-mode "lua-mode.el" "" t)
(autoload 'ruby-mode "ruby-mode.el" "" t)

(setq scroll-step 1)

(defun single-up ()   (interactive) (scroll-down 1))
(defun single-down () (interactive) (scroll-up 1))

(defun get-region-string ()
  (buffer-substring (region-beginning) (region-end)))

(defun fromhex-region ()
  "Convert selection from hex number to decimal number"
  (interactive)
  (let ((dec (string-to-number (get-region-string) 16)))
    (kill-region (region-beginning) (region-end))
    (insert (number-to-string dec))))

(defun eurodate ()
  "bhk ??-95: Generate DD-Mon-YY date string for today."
  (let ((time (current-time-string)))
    (concat
     (substring time 8 10) "-" (substring time 4 7) "-" (substring time -2))))

(defun dos()
  "Select DOS newlines (see 'unix')"
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos))

(defun unix()
  "Select UNIX newlines (see 'dos')"
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix))

(defun dump-env ()
  "Dump misc. environment information"
  (interactive)
  (dolist (e '(window-system (length (defined-colors)) process-environment system-configuration (version)))
       (insert (format "\n%s: %S" e (eval e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar in-mac
  (string-match "darwin" system-configuration)
  "t when running on a Mac (GUI or terminal)")

(defvar in-mac-gui
  (eq window-system 'ns)
  "t when running in EmacsForMacOSX GUI app")

;;;; Emacs.app (emacsformacosx.com)
;
; window-system: ns
; emacs-major-e
; (length (defined-colors)): 42
; process-environment: USER, SHELL, HOME, TERM=dumb
; system-configuration: "x86_64-apple-darwin"
; (version): "GNU Emacs 23.4.1 (x86_64-apple-darwin, NS apple-appkit-1038.36)\n of 2012-01-29 on bob.porkrind.org"

;;;; Linux, in SSH session from mintty in Cygwin 1.7

; window-system: nil
; (length (defined-colors)): 8
; process-environment: SSH_CONNECTION, SSH_TTY, SSH_CLIENT, "HOSTNAME=esxi-www.rwalker.com"
; system-configuration: "i686-pc-linux-gnu"
; (version): "GNU Emacs 23.1.1 (i686-pc-linux-gnu, GTK+ Version 2.18.9) of 2010-11-10 on c6b1.bsys.dev.centos.org"
;
;;;; Linux, in SSH session from Prompt (iPad)  [sends BACKSPACE!]



;; git uses less, which does the wrong thing in a dumb terminal
(setenv "PAGER" "cat")

;;;; emacsclient
;;  /usr/bin/emacs works with /usr/bin/emacsclient
;;  emacsformacosx.com works with its own emacsclient
(setenv "EDITOR" "emacsclient")
(server-start)


;; Mac apps run outside of bash or other user environment settings.
(when in-mac-gui
  (let ((app-bin "/Applications/Emacs.app/Contents/MacOS/bin")
        (PATH (getenv "PATH"))
        (HOME (getenv "HOME")))

    (change-font "hack13")

    (setenv "P4CONFIG" ".p4")

    ;; exec-path is used when launching programs.  aspell lives in ~/local/gin
    (add-to-list 'exec-path "/usr/local/bin")
    (add-to-list 'exec-path (concat HOME "/local/bin"))

    (setenv "PATH"
            (concat HOME "/bin:"
                    HOME "/local/bin:"
                    "/usr/local/bin:"
                    app-bin ":"          ; for emacsclient
                    PATH))

    (if (equal "/" default-directory)
        (setq default-directory
              (concat HOME "/")))))


;; Pick color scheme
(if in-mac-gui
    (colorscheme-green-on-black)
  (colorscheme-wheat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  Code-related utilities and conveniences  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How did I get by without this?  26-Oct-95 bhk
(defun other-window-prev ()
  "Select the previous window on this screen; same as (`other-window' -1), but
bound to an interactive function so it can be assigned to a key.
bhk 18-Jan-96"
  (interactive)
  (other-window -1))


(defun list-select (match lst)
  "Return new list of items from LST matching MATCH.
MATCH can be a function or a regexp."
  (let ((matchfn (if (stringp match)
		     (lambda (str) (string-match match str))
		   match))
	(result nil))
    (while lst
      (if (funcall matchfn (car lst))
	  (setq result (cons (car lst) result)))
      (setq lst (cdr lst)))
    (reverse result)))

;; Is buffer 'b' a buffer we want to browse with prev & next?
;;
(defun is-browsable-buffer (b)
  (let ((name (buffer-name b)))
    (not (string-match "^ ?\\*\\(Buff\\|Mess\\|scrat\\|Compl\\|Mini\\)" name))))

;;Was:    (not (string-match "^[ \*]" name))

;; And buffer history navigation   (Emissary-like)
;;   bury-buffer == go back in history
;;   next-buffer == go forward in history
(defun next-buffer ()
  "Select the 'next' buffer a la forward/backward (browser-like) buffer navigation.
bury-buffer and next-buffer act as 'back' and 'forward', respectively.
bhk Dec-95"
  (interactive)
  (switch-to-buffer (car (list-select
                          (lambda (f) (is-browsable-buffer f))  ;; TODO: how to get function value from name?
                          (reverse (buffer-list))))))

(defun prev-buffer ()
  "Select the 'previous' buffer a lt forward/backward browser-like buffer mavigation.
bury-buffer is almost what we want, but it tends to select a buffer distinct from all
other windows.  Sometimes we want to see the same file in two windows."
  (interactive)
  (let ((head (car (buffer-list))))
    (bury-buffer (car (buffer-list)))
    (while (and (not (eq head (car (buffer-list))))
                (not (is-browsable-buffer (car (buffer-list)))))
      (bury-buffer (car (buffer-list)))))
  (switch-to-buffer (car (buffer-list))))


;;  c:/p/util/grep can handle "/path/*/*.h" type expressions; c:/p/gnu/bin/grep cannot.
;;  c:/p/util/grep cannot handle long file names, but that's okay for pilot-grep,
;;    since we have to use 8.3 names because next-error can't either (see below),
;;    so I'll stick with it here.

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

;; Conflicts between expressions in compilation-error-regexp-alist are not
;; handled well, so we carefully select the ones we want.  In particular,
;; the 'gnu' rule conflicts with Lua error messages.  We cannot set these
;; variables before (require 'compile), and compilation-mode-hook is
;; apparently too late, so we require it now.
;;
;; Test this by loading ./mycomp.txt
;;
(defvar my-regexp-alist-alist
  (eval-when-compile
    (let* ((file "\\(\\(?:[a-zA-Z]:[/\\\\]\\|[/\\\\]\\)?[\\.[:alnum:]][^():\n]*[[:alpha:]]\\)")

           ;; Match "<digits>[.<digits>]" ; two captures: line, column
           (lc "\\([0-9]+\\)\\(?:[:.]\\([0-9]+\\)\\)?")

           ;; Match "<file>:<lc>[-<lc>]"
           ;;   Five captures:  file, line1, col1, line2, col2
           ;;   Skip initial "at " or "In file included from "
           (colons (concat "\\(?:^\\|[[:space:]:]\\)\\(?:at \\|In file included from \\)?" file ":" lc "\\(?:-" lc "\\)?:.*"))

           ;; Match "<file>(<line>)..." ; two captures: file, line
           (parens (concat "^" file "(\\([0-9]+\\))[ :]*[^:]*\\(?:[Ww]arning\\|[Ee]rror\\)"))

           ;; Match "<file>: line <line> ..."  (but not when file is "/bin/bash")
           (mybash "^\\([^:\n/][^:\n]+\\): line \\([0-9]+\\):")

           ;; Match ""<file>", line <line> ..."
           (armcc "\"\\([^\"\n]+\\)\",? [Ll]ine \\([0-9]+\\)")

           ;; Match <assert.h> "Assertion failed: ..."
           (asrt "^Assertion failed: .*, file \\([^,]*\\), line \\([0-9]+\\)")

           ;; Match "   at XXXX (<file>:<lc>)"
           (node (concat "^    at .* (\\(/[^()\n:]*\\):" lc ")"))
           (node2 (concat "^\\(/[^():\n]*\\.[a-zA-Z]+\\):" lc)))

      `((node ,node 1 (2 . 4) (3 . 5) nil 1)
        (node2 ,node2 1 (2 . 4) (3 . 5) nil 1)
        (colons ,colons 1 (2 . 4) (3 . 5) nil 1)
        (asrt ,asrt 1 2 nil nil 1)
        (parens ,parens 1 2 nil nil 1)
        (mybash ,mybash 1 2 nil nil 1)
        (armcc ,armcc 1 2 nil nil 1)))))

(eval-when-compile
  (require 'compile))

(when (boundp 'compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist-alist
	(append my-regexp-alist-alist compilation-error-regexp-alist-alist))
 (setq compilation-error-regexp-alist '(asrt colons parens mybash armcc node node2
						gcc-include gcov-file gcov-header gcov-nomark
						gcov-called-line gcov-never-called)))


;; shell
(defun shell-bottom (arg)
  (interactive "p")
  (if (eq arg 1)
      (shell)
    (shell (concat "*AltShell-" (number-to-string arg) "*")))
  (goto-char (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;     Hooks, Bindings, Modes    ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Revealing cryptic emacs key binding syntax:
;;   1. Use global-set-key to bind a key
;;   2. repeat-complex-command (C-x ESC eSC)

;; Mac OSX Terminal notes:
;;   alt/option + left  =>  ESC left   (NOT M-left)
;;   fn + pretzel + Fx  =>  Fx

;; see CC mode info
(eval-when-compile (require 'cc-mode))
(defun my-c-mode-common-hook ()
  (setq tab-width 8
        indent-tabs-mode nil        ; Use spaces instead of tabs
        c-basic-offset 2)           ; mother of all indentation parameters
  (turn-on-font-lock)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq auto-mode-alist
      (append '(("\\.[ch]\\'" . c++-mode)
		("\\.bid\\'" . c++-mode)
                ("\\.min\\'" . makefile-gmake-mode)
                ("\\.mak\\'" . makefile-gmake-mode)
                ("\\.mk\\'" . makefile-gmake-mode)
                ("[Mm]akefile\\'" . makefile-gmake-mode)
                ("scam\\'" . makefile-gmake-mode)
                ("SConscript\\'" . python-mode)
                ("Package\\'" . makefile-gmake-mode)
                ("^\\.config\\'" . makefile-gmake-mode)
                ("\\.lua\\'" . lua-mode)
                ("^pak\\'" . lua-mode)
                ("\\.cif\\'" . lua-mode)
                ("\\.rb\\'" . ruby-mode))
              auto-mode-alist))

(defun my-limode-hook ()
  (local-set-key [C-return] 'eval-last-sexp))
(add-hook 'lisp-interaction-mode-hook 'my-limode-hook)

;; js2.el
;(autoload 'js2-mode "js2" nil t)
;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Ensure the subcommands (e.g. make spawned from 'compile') get my user
;; environment settings, even on MacOS or Windows, where the GUI version of
;; emacs is not spawned from a shell.
;;
(setenv "BASH_ENV" (concat (getenv "HOME") "/.bashrc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cwdtrack: like dirtrack, but not brain dead.  dirtrack hoses shell mode
;; badly if the prompt regexp matches something that isn't a directory.
;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save-and-make: faster, better, cheaper

(defvar save-and-make-command "make")
(defun save-and-make (arg)
  (interactive "P")
  (if (not (equal "*" (substring (buffer-name) 0 1)))
      (save-buffer))
  (if arg
      (setq save-and-make-command (read-string "Make command: " save-and-make-command)))
  (compile save-and-make-command))


;; "GDB mode"
(defun my-gud-mode-hook ()
  (global-set-key [?\C-c ?\C-,] 'gud-up)
  (global-set-key [?\C-c ?\C-.] 'gud-down))

(add-hook 'gud-mode-hook 'my-gud-mode-hook)

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
(global-set-key [f1] 'find-tag)
(global-set-key [f2] 'tags-search)
(global-set-key [f3] 'tags-loop-continue)
(global-set-key [f7] 'compile)
(if (>= emacs-major-version 23)
    (progn
      (global-set-key [?\C-=] 'text-scale-increase)
      (global-set-key [?\C--] 'text-scale-decrease))
  (require 'fontsize)
  (global-set-key [?\C-=] 'fontsize-up)
  (global-set-key [?\C--] 'fontsize-down))

(global-set-key [deletechar] 'backward-delete-char)  ;; VT100 sequence?

;; Use C-z for private keymap (more portable than F-keys)
;;
(global-set-key "\C-z" nil)  ;; suspend-emacs not that useful
(global-set-key "\C-z\C-m" 'save-and-make)
(global-set-key "\C-zm" 'compile)
(global-set-key "\C-z\C-n" 'next-error)
(global-set-key "\C-z\C-p" 'previous-error)
(global-set-key "\C-z\C-s" 'shell-bottom)
(global-set-key "\C-z\C-t" 'ansi-term)
(global-set-key "\C-z\C-f" 'tree-find-file)
(global-set-key "\C-z\C-g" 'tree-grep)
(global-set-key "\C-z\C-h" 'tree-grep-headers)
(global-set-key "\C-z\C-j" 'tree-hyperjump)
(global-set-key "\C-zg"    'c-grep)
(global-set-key "\C-z\C-o" 'p4-offline)


;; The dreaded "delete sends ^H" problem...
(defun backspace-is-delete ()
  (interactive)
  (global-set-key "\C-h" 'backward-delete-char))

(when (not (or window-system
               (equal (getenv "TERM") "xterm-color")
               (equal (getenv "TERM") "xterm-256color")))
  (backspace-is-delete))

(global-set-key "\C-z\C-h" 'backspace-is-delete)


;; Horizontal scrolling (see toggle-truncate-lines)
(global-set-key [?\C-,] (lambda () (interactive) (scroll-left 4)))
(global-set-key [?\C-.] (lambda () (interactive) (scroll-right 4)))

(global-unset-key "\C-xnn") ; narrow-to-region

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] 'other-window-prev)

(global-set-key [mouse-3] 'tree-mouse-hyperjump)

(put 'eval-expression 'disabled nil)    ; M-:
(custom-set-variables
 '(ps-line-number t)
 '(indent-tabs-mode nil)
 '(ps-number-of-columns 2)
 '(ps-landscape-mode t)
 '(fill-column 76))

(setq transient-mark-mode t)
(setq truncate-partial-width-windows nil)
(setq search-highlight t)
(delete-selection-mode t)

;; Delete trailing whitespace from lines before a file is saved.
;; (unless we're editing a makefile)
;;
(defun clean-whitespace ()
  (unless (string-match "^make" (symbol-name major-mode))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'clean-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  Local definitions  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if my-host
    (load (concat "~/.emacs-" my-host) t t))

(defun my-artist-mode-hook ()
  (when (boundp 'artist-mode-map)
    ;; button 3 = right button on two-button mouse
    (define-key artist-mode-map [down-mouse-3] 'artist-mouse-choose-operation)))

(add-hook 'artist-mode-init-hook 'my-artist-mode-hook)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;(defvar ps-printer-name "\\\\rejectamenta\\ax110amfd")
;;(setq printer-name "\\\\rejectamenta\\ax110amfd")

;(defun start-irc ()
;   "Connect to IRC."
;   (interactive)
;   (require 'erc)
;   (when (fboundp 'window-system)
;     (setq erc-autojoin-channels-alist '((".*\\.qualcomm\\.com" "#minkapi"))))
;   (erc-tls :server "chat-irc-prod.qualcomm.com" :port "9999" :nick "bhk" :full-name "Brian Kelley"))
