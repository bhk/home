;; Old fragments of elisp code, not currently used

(require 'benchmark)
(benchmark-elapse (length (treespec-find-files "~/p4/apiOne"))) ;; .396, .397, .398

(treespec-parse (get-file-as-string "~/p4/apiOne/.treespec") "~/p4/apiOne")


;; Experiments with optimization of evalutated forms:
;;
;; optimize-form: Collapse nested and/or:  (and a (and b c))  -> (and a b c)
;;
;; This does not understand control structures.;
;;
;; Note: byte-optimize-form (available in Carbon emacs) reduces (or x) to x
;; and (or nil ...) to (or ...).

(defun optimize-form (form)
  (when (consp form)
    (setq form (mapcar 'optimize-form form))
    (let* ((op (and (consp form) (car form)))
	   (opinfo (assq op '((and . t) (or . nil)))))
      (when opinfo
	(let ((elem form)
	      (idval (cdr opinfo)))
	  (while (cdr elem)
	    (let ((arg (cadr elem)))
	      ;; Collapse (op (op x) y)  -> (op x y)
	      (if (eq op (and (consp arg) (car arg)))
		  (setcdr elem (append (cdr arg) (cddr elem)))
		;; Discard (and .. t ..) -> (and ....)
		(if (eq idval arg)
		    (setcdr elem (cddr elem))
		  (setq elem (cdr elem))))))
	  ;; Reduce (and) -> t; (or) -> nil
	  (if (null (cdr form))
	      (setq form idval)
	    ;; Reduce (op x) -> x
	    (if (null (cddr form))
		(setq form (cadr form))))))))
  form)


;; * For for following fairly-complicated example, the following
;;   optimizations pay form themselves after...
;;      optimize-form: ~ 150 reps
;;      byte-compile:  ~ 650 reps
;; * In tree example, ~6000 files scanned (apprx) with ~3200 matches:
;;      total time                400 ms
;;      (funcall fno ...) :        12 ms  (apprx)
;;      potential fnoc savings:     2 ms
;;
(let* ((a t) (b t)
       (frm '(and (and a (and a b) (and a b) b)
		  (and (and a b) b (and b t))
		  (or (or a nil) (or b (or a (or b (or a (or b nil))))))))
       (frmo (optimize-form frm))
       (fn (lambda (a) (eval frm)))
       (fno (lambda (a) (eval frmo)))
       (fnb (lambda (a) (eval frmb)))
       (fnoc (byte-compile `(lambda (a) (eval ,frmo)))))  ; must expand form inline
  (list
   ;; Benefits?
   (benchmark-elapse (dotimes (x 1000) (funcall fn t)))    ;; 2.9 microsecond/rep
   (benchmark-elapse (dotimes (x 1000) (funcall fno t)))   ;; 2.1
   (benchmark-elapse (dotimes (x 1000) (funcall fnoc t)))  ;; 1.7
   ;; Costs?
   (benchmark-elapse (dotimes (x 1000) (optimize-form frm)))  ;;  120 us
   (benchmark-elapse
     (dotimes (x 1000) (byte-compile `(lambda (a) (eval ,frmo)))))))  ;; 355 us





;; (defun check-tabs ()
;;   (save-excursion
;;     (and (goto-char (point-min))
;; 	 (re-search-forward "tabs=\\([0-9]\\)" 80 t)
;; 	 (let ((wid (string-to-number (match-string 1))))
;; 	   (setq tab-width wid)
;; 	   (message "Using tab width %d in this buffer." wid))))
;;   nil)
;;(add-hook 'find-file-hooks 'check-tabs)


(defun insert-copyright (file)
  (beginning-of-buffer)
  (insert-file-contents file)
  (when (search-forward "File" 400 t)
    (end-of-line)
    (insert (file-name-nondirectory buffer-file-name)))
  (beginning-of-buffer)
  (when (search-forward "Date" 400 t)
    (end-of-line)
    (insert (eurodate)))
  (beginning-of-buffer)
  (search-forward "Description")
  (end-of-line))

(defun copr ()
  "Prepend copyright notice to source file."
  (interactive)
  (insert-copyright "~/p/dev/env/copr.txt"))


(defun list-to-lines (lst)
  "Convert strings in a list to lines of text in a string."
  (mapconcat 'identity lst "\n"))


(defun myfind (dir match)
  "Display files under a base directory matching a regular expression."
  (interactive "DFind base: \nsMatch regexp: ")
  (switch-to-buffer (generate-new-buffer (concat "*find*")))
  (insert (format "find %s in %s:\n\n" match dir))
  (insert (list-to-lines (find-files dir match)))
  (insert "\n")
  (beginning-of-buffer))


;; If no thing at point, search for thing near
(defun bounds-of-thing-near-point (th)
  (let ((b (bounds-of-thing-at-point th)))
    (if (equal (car b) (cdr b))
	(save-excursion
	  (forward-word 1)
	  (bounds-of-thing-at-point th))
      b)))

(defun thing-at-point-no-properties (th bnear)
  "Like thing-at-point without text properties, and optional search forward."
  (let ((b (if bnear (bounds-of-thing-near-point th)
	     (bounds-of-thing-at-point th))))
    (buffer-substring-no-properties (car b) (cdr b))))

(defun regexp-for-extensions (exts)
"Return a regular expression for a set of file extensions.
EXTS is a comma-delimited list of extensions (ex: \"c,h\").  The
resulting regex will be case-insensitive (a concession to DOS)."
  (let ((elist (split-string (concat (upcase exts) "," (downcase exts)) ",")))
    (concat "\\." (regexp-opt elist t) "$")))

(defun printf (&rest args)
  (print (apply 'format args)))

(defun filter (fn lst)
  "Return all members of LST for which FN return non-nil"
  (let (result)
    (dolist (a lst)
      (if (funcall fn a)
	  (push a result)))
    (reverse result)))

;;(test "filter" (equal (filter (lambda (a) (> a 4))  '(1 5 3 7))
;;                      '(5 7)))


(defun foldr (fun val list)
  "Return (fun firstitem (fun seconditem (... (fun lastitem val) ...)))"
  (if list
      (funcall fun (car list) (foldr fun val (cdr list)))
    val))

(defmacro lfoldr (a b val list &rest body)
  "(lfoldr a b val list ...) is shorthand for (foldr (lambda (a b) ...) val list)"
  `(foldr (lambda (,a ,b) ,@body) ,val ,list))

(defun mapchain (func initvalue lst)
  "Apply a function to each item in LST, chaining results.
Calls (FUNC chainvalue item) once for each item, where chainvalue the value
returned from the prior call to FUNC, or INITVALUE when the first item is
processed.  Returns the value returned by the last call to FUNC, or INITVALUE
if LST is empty."
  (while lst
    (setq initvalue (funcall func initvalue (car lst)))
    (setq lst (cdr lst)))
  initvalue)

(defun mapmapcar (func seq seq2)
  "Apply FUNCTION to all permutations of elements of SEQ and SEQ2.
SEQ2 is the innermost loop.
Example: (mapmapcar \'concat \'(\"a\" \"b\") \'(\"1\" \"2\"))
          => ((\"a1\" \"a2\") (\"b1\" \"b2\"))"
  (mapcar (lambda (arg)
	    (mapcar (lambda (arg2)
		      (funcall func arg arg2))
		    seq2))
	  seq))

;; Mostly redundant with (replace-regexp-in-string from to str)
(defun string-replace (str from to)
  "Create new string from STR, substituting TO for occurrences of
regex FROM."
  (let ((pos 0)
	(len (length str))
	(result ""))
    (while (< pos len)
      (let* ((matchpos (string-match from str pos))
	     (prematch (substring str pos matchpos)))
	
	(if matchpos
	    (progn
	      (setq result (concat result prematch to))
	      (setq pos (match-end 0)))
	  (progn
	    (setq result (concat result prematch))
	    (setq pos len)))))
    result))


(defun subdirs (dir)
  "Return a list of the subdirectories of DIR (string)."
  (let (result)
    (dolist (f (directory-files dir t "[^.]" t))
      (if (file-accessible-directory-p f)
	  (push f result)))
    result))


(defun nestdirs (dirs)
  "Return a list of all directories at or under DIRS."
  (let ((dirlist (listify-if dirs))
	(result nil))
    (while dirlist
      (dolist (f (directory-files (pop dirlist) t "[^.]" t))
	(when (file-accessible-directory-p f)
	  (push f dirlist)
	  (push f result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes on eval & dyamic vs. lexical coping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What is the scope for free variables in an eval'ed expression?  Where are
;; free variables found?  In a dynamically-scoped langauge, it can be the same
;; as the scope in which 'eval' is called:

(let ((form '(* x 2))
      (x 5))
  (eval form))
;; Emacs lisp -> 10


;; We can, of course, avoid free variables by making it a function:
(let ((fn (eval '(lambda (x) (* x 2))))
      (x 3))
  (funcall fn 4))
;; -> 8

(require 'tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proj-*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Example project definitions:
;;
;;   (proj-add "SDK" "/dev/brew/"
;;             "/dev/brew/sdk/TAGS" "/dev/brew/")
;;
;;   (defun fooproj-activate ()
;;     (setq c-basic-offset 3))
;;   (proj-add "FOO" "/dev/foo/"
;;             "/dev/foo/TAGS" "/dev/foo/"
;;             '(activate . fooproj-activate))
;;
;; - Emacs tags automatically selects among several tags files in the tags
;;   table list based on the current file.  Setting the tags table
;;   explicitly via the proj-activate command could be useful with
;;   overlapping projects.

(defvar gentags-exts "c,h,cpp,lua,el,bid,cif"
  "Comma-delimited list of extensions for TAGS")

(defun gentags-files (dirs)
  "Show all files under DIRS that will be used for tags."
  (find-files dirs (regexp-for-extensions gentags-exts)))

(defun gentags (tags dirs)
  "Regenerate TAGS file from source files in DIRS."
  (generate-tags tags (gentags-files dirs)))

;;
;; By default, proj-find hooks find-file
;;
(add-hook 'find-file-hooks 'proj-find-hook)

(defvar proj-alist ()
  "Association-list of project descriptions.  Each key is a string
that names the project, and each value is a project description.
Each project description is in turn an alist, with the following
keys defined:
   
   files, tags, tagdirs: see `proj-add'
   activate: function to be called when project is activated (invoked
             with one parameter: the project description itself)

The function `proj-find-hook' automatically activates projects when
`find-file' is invoked.  You can use `proj-add' to add entries.")

(defvar proj-current nil
  "Currently-activated project description.")

(defvar proj-read-file-alist nil
  "Cache used by `proj-read-file-by-name'.")

(defun proj-includes (pdesc file)
  "See if FILE is included in the files of PDESC.  If found, returns
matching regexps from the 'files' value of PDESC."
  (list-select (lambda (regex)
		 (string-match regex file))
	       (listify-if (assqval 'files pdesc))))

(defun proj-add (name files tags tagdirs &rest misc)
  "Add a project description for NAME to proj-alist.

    NAME     = project name
    FILES    = regexp (or list of regexps) matching full paths of files
               that ctivate the project when visited.
    TAGS     = TAGS file name to be selected with project is activated
    TAGDIRS  = string or list of directories containing sources for
               the TAGS files (also used for proj-find-file)
    MISC     = alist to be added to the project description.  See 
               `proj-alist' for more info."
  (add-to-list 'proj-alist
	       (cons name
		     (nconc (list (cons 'files files)
				  (cons 'tags tags)
				  (cons 'tagdirs tagdirs))
			    misc))))

(defun proj-select (arg prompt)
  "Select project name.  ARG typically represents a command prefix arg.
If ARG is nil, use the active project, or query user if none are active.
If ARG is a string, return ARG.
Otherwise (ARG specified, but not a string), always query user."
  (if (stringp arg)
      arg
    (if (and (null arg)
	     (car proj-current))
	(car proj-current)
      ;; else query user
      (let ((completion-ignore-case nil)	;; local binding visible to completing-read == "ambient value"?
	    ;; Default to a project not currently active
	    (default (if proj-current
			 (caar (list-select (lambda (proj) (not (eq proj-current proj)))
					    proj-alist))
		       "")))
	(completing-read prompt proj-alist nil t default)))))

(defun proj-activate (proj)
  "Activate a project, if not already active.  PROJ can be the name of
a project in `proj-alist' or a project description (alist).
If PROJ is specified by name (as when invoked interactively), all other
tags-table-list entries will be removed."
  (interactive
   (list (proj-select t "Activate project: ")))
  (when (stringp proj)
    (setq proj (assoc proj proj-alist))
    (setq tags-table-list nil))
  (if proj
      (let* ((name (car proj))
	     (tags (assqval 'tags proj))
	     (fn   (assqval 'activate proj)))
	(if (equal (car proj-current) name)
	    (message "File in current project.")
	  (progn
	    (message "Using project: %s" name)
	    (setq proj-current proj)
	    (if tags
		(add-to-list 'tags-table-list tags))
	    (if fn
		(funcall fn proj)))))))

(defun proj-find-hook (&optional file noactivate)
  "Activate project associated with file in current buffer, if matching
project is found, and if project is not already active."
  (interactive)
  (or file
      (setq file buffer-file-name))
  (let ((plist  proj-alist))
    ;; Look for first match (or end of list)
    (while (and plist
		(not (proj-includes (car plist) file)))
      (setq plist (cdr plist)))
    (or noactivate
	(proj-activate (car plist)))
    ;; return project name, if any
    (caar plist)))

(defun proj-list-files (&optional projname)
  "Display in a new buffer all files used in generating TAGS for the current
or specified project, or PROJNAME if specified.  If PROJNAME is a numeric
argument, query user for project name.  (See `proj-alist'.)"
  (interactive "P")
  (let* ((projname (proj-select projname "List files in project: "))
	 (proj (assoc projname proj-alist))
	 (files (gentags-files (assqval 'tagdirs proj))))
    (if files
	(progn
	  (switch-to-buffer
	   (generate-new-buffer (concat "*" (car proj) " project files*")))
	  (insert (list-to-lines files))
	  (beginning-of-buffer)
	  nil)
      (if projname
	  (format "** Project not found: %s" projname)
	"** No current project"))))

(defun proj-gentag (arg)
  "Regenerate TAGS file for currently-active project.  If no project is active
or if a prefix argument is specified, query user for project name. (See
`proj-alist'.)"
  (interactive "P")
  (let* ((projname (proj-select arg "Generate TAGS for project: "))
	 (proj (assoc projname proj-alist))
	 (tags (assqval 'tags proj))
	 (dirs (assqval 'tagdirs proj)))

    (if (and tags dirs)
	(gentags tags dirs)
      (message "** No retag info for proj-current."))))

(defun proj-read-file-by-name (projname prompt &optional fname)
  "Get name of file in project named PROJNAME, allowing user to specify by
file-then-directory with completion."
  ;; Collect all tagged project files in "file [path]" form...
  (if (not (equal (car proj-read-file-alist) projname))
      (setq proj-read-file-alist
	    (cons projname (mapcar (lambda (f)
				     (cons (file-name-path f) f))
				   (gentags-files (assqval 'tagdirs
							   (assoc projname proj-alist)))))))
  ;; Query "name [path]"

  ;; alist consists of  ( ("name path" . "path/name") ("name path" . "path/name") ...)
  (let ((alist (cdr proj-read-file-alist)))
    ;; narrow list with fname arg?
    (if fname
	(setq alist (list-select
		     (lambda (a) (string-match (concat (regexp-quote fname) "$") (cdr a)))
		     alist)))
    (if alist
	(if (not (cdr alist))
	    ;; one entry => use it
	    (cdar alist)
	  ;; else query user
	  (let ((input (completing-read prompt alist nil t)))
	    ;; Look up and return dir/name  
	    (cdr (assoc input alist)))))))

(defun proj-find-file (proj &optional fname)
  "Find file in current project.  Query user by filename (not directory) with
completion.  If no project is active, or if a numeric prefix argument is
specified, query user for project name first.  A string prefix specifies the
project name directly.  (See `proj-alist'.)"
  (interactive "P")
  (let ((projname (proj-select proj "Find file in project: ")))
    (find-file (proj-read-file-by-name projname (format "Find file (in %s): " projname) fname))))

(defun proj-mouse-hyperjump (e)
  "Visit filename when user clicks on it.  This is designed to be bound
to a mouse event, as in:
	(global-set-key [mouse-3] 'proj-mouse-hyperjump)
Filename delimiters are defined by `thing-near-point'.
Relative of absolute paths can be specified.  If a file is not found relative
to the current directory, the current project will be searched.  (See
`proj-find-file'."
  (interactive "e")
  (let* ((clickcount (car (cddr e)))
	 (posinfo (cadr e))
	 (window (car posinfo))
	 (bufpos (cadr posinfo))
	 (buffer (window-buffer window))
	 (filename (save-excursion
		     (set-buffer buffer)
		     (goto-char bufpos)
		     (thing-at-point-no-properties 'filename nil))))
    ;; go to file if it exists
    (if (not (equal "" filename))
	(if (file-exists-p filename)
	    (find-file filename)
	  (proj-find-file nil filename)))))

(defun proj-hyperjump ()
  "Visit file named by filename at point."
  (interactive)
  (let ((filename (thing-at-point-no-properties 'filename t)))
    (or (equal "" filename)
	(if (file-exists-p filename)
	    (find-file filename)
	  (proj-find-file nil filename)))))

