;;; yyorg-bookmark.el --- yy's bookmark manage tool -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'server)

(require 'org)
(require 'org-capture)
(require 'org-protocol)
(require 'org-attach)

;; basic options: some infomation about enchant file
(defvar t-headline "YYOB-MANAGEMENT"
  "enchant file's manage headline's name")
(defvar t-global-counter-name "YYOB-COUNTER"
  "name of enchant file's global counter")
(defvar t-enchant-name "enchant.org"
  "enchant file's name")
(defvar t-enchant-file
  (if load-in-progress
      (concat (file-name-directory load-file-name) t-enchant-name)
    (expand-file-name t-enchant-name))
  "the full path of enchant file.
if use `require', then use load-file-name's directory.
else use current buffer's path")
;; make sure enchant file's existance
(cl-assert (file-exists-p t-enchant-file))

(defvar t-wget-path nil
  "the file path of wget.exe
wget is a free software package for retrieving files using HTTP, HTTPS, FTP and FTPS")

(defun t-enchant ()
  "add template file to the end of current buffer
the enchant file includes some file-local variables,
so it is needed to be added to the end of file"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (insert (with-temp-buffer
		(insert-file-contents t-enchant-file)
		(buffer-string))))))

(cl-defun t-add-template (&key key desc type target temp props)
  "key is a one char sting
desc is a description string
type is entry's type, a symbol
target is a list
temp is the template string
props is the properties plist"
  (cl-assert (stringp key))
  (cl-assert (stringp desc))
  (cl-assert (member type '(entry item checkitem
				  table-line plain)))
  (cl-assert (consp target))
  (cl-assert (stringp temp))
  (cl-assert (listp props))
  (let ((item (cl-find-if (lambda (x) (string= key (car x)))
			  org-capture-templates)))
    (cond
     ((not item)
      (add-to-list 'org-capture-templates
		   `(,key ,desc ,type ,target ,temp ,@props))
      (message "add new template item %s" key))
     ((y-or-n-p (format "item %s exists, overwrite it?(y/n)" key))
      (setq org-capture-templates
	    (cl-delete-if (lambda (x) (string= key (car x)))
			  org-capture-templates))
      (add-to-list 'org-capture-templates
		   `(,key ,desc ,type ,target ,temp ,@props))
      (message "update template item %s" key))
     (t (message "don't add new template item %s" key)))))

(defun t--target-filename (target)
  "get file path from the `target' list
according to different kinds of target"
  (cl-case (car target)
    ((file file+headline file+olp file+regexp
	   file+olp+datatree file+function)
     (let ((file (cadr target)))
       (if (string= file "") org-default-notes-file file)))
    ((id) (let ((id-file (org-id-find (cadr target))))
	    (if (null id-file) nil
	      (expand-file-name (car id-file)))))
    ((clock) (if (not (org-clocking-p)) nil
	       (let ((buf (org-clock-is-active)))
		 (buffer-file-name buf))))
    ((function) (let ((old-buf (current-buffer)))
		  (funcall (cadr target))
		  (let ((file-name (buffer-file-name)))
		    (switch-to-buffer old-buf)
		    file-name)))
    (t nil)))

(defun t--template-filename (key &optional tplist)
  "get filename of a key from `tplist', return nil if not found
if tplist is not provided, use org-capture-templates"
  (cl-assert (stringp key))
  (let* ((alist (or tplist org-capture-templates))
	 (tp (assoc key alist)))
    (if (not tp) nil
      (let ((target (cadddr tp)))
	(t--target-filename target)))))

;; unused function
;; maybe useful after capture and before C-c C-c, but I think it is useless
;; reserve just because I'm lazy
(defun t--current-target-filename ()
  "get current target's filename
use the plist `org-capture-current-plist'"
  (let ((target (org-capture-get :target t)))
    (t--target-filename target)))

(defun t--template-keys (&optional tplist)
  "get keys of all templates"
  (let ((alist (or tplist org-capture-templates)))
    (mapcar (lambda (item) (car item)) alist)))

(defun t-remove-template (key)
  "remove a template from `org-capture-templates'
use minibuffer to select a key"
  (interactive (list (completing-read "key: " (t--template-keys)
				      nil t)))
  (setq org-capture-templates
	(cl-delete-if (lambda (x) (string= key (car x)))
		      org-capture-templates)))

;; you can also use org-find-exact-headline-in-buffer
;; to locate a headline with the property you want

(defun t--get-property (pname &optional on-headline)
  "return string if found, or nil if not"
  (let ((place (if on-headline (point)
		 (org-find-property pname))))
    (if place (org-entry-get place pname) nil)))

(defun t--set-property (pname strval &optional on-headline)
  "set property `pname' if found and return t, or nil if not
if on-headline is set and point is on headline
this function will always success"
  (let ((place (if on-headline (point)
		 (org-find-property pname))))
    (if place (prog1 t (org-entry-put place pname strval)) nil)))

;; gv setter for t--get-property
(gv-define-setter t--get-property
    (value pname &optional on-headline)
  `(t--set-property ,pname ,value ,on-headline))

;; str incf operator
(defmacro t--incfstr (place &optional n)
  "string version of cl-incf"
  (gv-letplace (ge se) place
    (macroexp-let2 nil v (or n 1)
      (funcall se `(number-to-string
		    (+ (string-to-number ,ge) ,v))))))

(defun t-increase-counter (pname &optional on-headline)
  "increase property `pname' by 1
return the origin value"
  (let ((a (t--get-property pname on-headline)))
    (prog1 a
      (t--incfstr a)
      (t--set-property pname a on-headline))))

(defun t-decrease-counter (pname &optional on-headline)
  "decrease property `pname' by 1
return the origin value"
  (let ((a (t--get-property pname on-headline)))
    (prog1 a
      (t--incfstr a -1)
      (t--set-property pname a on-headline))))

(defun t-reset-counter (pname &optional on-headline)
  "set counter to 0
return the origin value"
  (let ((a (t--get-property pname on-headline)))
    (prog1 a
      (t--set-property pname "0" on-headline))))

(defun t-control-counter (pname op &optional on-headline)
  "control counter's value
'+ is add1, '- is sub1, 'r is reset to 0, 'z is unchange
return the origin value"
  (cl-case op
    ((+) (t-increase-counter pname on-headline))
    ((-) (t-decrease-counter pname on-headline))
    ((r) (t-reset-counter pname on-headline))
    ((z) (t--get-property pname on-headline))
    (t (error "unrecognized op %s" op))))

(defun t-control-global-counter (&optional op)
  "control the global counter in bookmark files"
  (let ((op (or op '+)))
    (t-control-counter t-global-counter-name op)))

(defun t-control-key-counter (key &optional op)
  "used in capture template
use key in org-capture-template to locate file"
  (let* ((filename (t--template-filename key))
	 (buf (get-file-buffer filename)))
    (with-current-buffer buf
      (t-control-global-counter op))))

;; properties opeartions
(defun t-get-all-entries-properties (pnames)
  "get all entries specific property
return form is ( ((p1 . v1) (p2 . v2) ...) ... )
in other words, return value is a nested alist
you can use it with narrow"
  (let ((pro-list))
    (org-map-tree
     (lambda ()
       (let ((a (org-entry-properties))
	     (b))
	 (mapc (lambda (x) (let ((c (assoc x a)))
			     (when c (push c b))))
	       pnames)
	 (when b (push b pro-list)))))
    (reverse pro-list)))

(defun t-get-all-entries-properties-under-headline (pnames uheadline)
  "get all entries specific proeprties, under an unique headline"
  (save-excursion
    (save-restriction
      (let ((pos (org-find-exact-headline-in-buffer uheadline)))
	(if (not pos) (error "headline not found")
	  (goto-char pos)
	  (org-narrow-to-subtree)
	  (yyorg-bookmark-get-all-entries-properties pnames))))))

;; buffer-local variables operations
(defun t-get-local-value (key symbol)
  "get buffer-local value in target file"
  (let* ((filename (t--template-filename key))
	 (buf (get-file-buffer filename)))
    (save-current-buffer
      (set-buffer buf)
      (symbol-value symbol))))

(defun t-set-local-value (key symbol value)
  "set buffer-local value in target file"
  (let* ((filename (t--template-filename key))
	 (buf (get-file-buffer filename)))
    (save-current-buffer
      (set-buffer buf)
      (set symbol value))))

;; gv for t-get-local-value :p
(gv-define-setter t-get-local-value (value key symbol)
  `(t-set-local-value ,key ,symbol ,value))

;; macro for doing sth on target buffer's context
(defmacro t-with-current-key-buffer (key &rest body)
  "use key's file as current buffer
and do something, return the last expression's value"
  `(with-current-buffer (get-file-buffer (t--template-filename ,key))
     ,@body))

;; download related operations

;; used for tag repeated items
(defun t-add-repeat-tag (key table lookfn)
  "return \":repeat:\" if key is in table
otherwise a null string \"\""
  (let ((res (funcall lookfn key table)))
    (if (null res) "" ":repeat:")))

(defun t-get-url-from-link (str)
  "get link from [[link][description]]"
  (cl-assert (string= (substring str 0 2) "[["))
  (let ((i 0))
    (while (and (not (= (aref str i) ?\]))
		(< i (length str)))
      (cl-incf i))
    (if (= i (length str)) (error "link not found")
      (substring str 2 i))))

;; https://stackoverflow.com/questions/13505113/how-to-open-the-native-cmd-exe-window-in-emacs
;; https://www.tecmint.com/wget-download-file-to-specific-directory/
;; https://www.anycodings.com/1questions/2463613/is-it-possible-for-wget-to-flatten-the-result-directories

(defun t-attach-use-wget (link)
  "-E -H -k -K -p -nd -e robots=off
-P target-directory
used only on windows
just to modify cmd to bash and something else to adapt to linux
or use advice, 好きにしなよ"
  (let* ((dir-path (org-attach-dir-get-create))
	 (wget-exe (or t-wget-path "wget")))
    (let ((proc (start-process "yyob-wget" nil
			       "cmd.exe" "/C" "start" "cmd.exe"
			       "/K"
			       wget-exe "-E" "-k" "-K" "-p"
			       "-nd" "-e" "robots=off"
			       link
			       "-P" dir-path)))
      (set-process-query-on-exit-flag proc nil))))

(defun t-attach-use-wget-on-headline ()
  "interface function
headline's item must be the form of [[link][desc]] ...."
  (interactive)
  (let* ((item (org-entry-get (point) "ITEM"))
	 (link (t-get-url-from-link item))
	 (tags (org-get-tags)))
    (unless (member "ATTACH" tags)
      (org-set-tags (cons "ATTACH" tags)))
    (t-attach-use-wget link)))

;; emacsclient server operations

;; start server if not start
(unless (eq (server-running-p) t)
  (server-start))

;; restart server function
;; sometimes emacsclient doesn't work due to some unknown problems
;; so let's just shutdown it and restart
(defun t-restart-server ()
  "restart emacs server unconditionally"
  (interactive)
  (if (not (eq (server-running-p) t)) ; server not run
      (progn (server-start)
	     (message "emacs server starts now"))
    (server-force-stop)
    (server-start)
    (message "emacs server restarts now")))

;; buffer-local keymap use minor-mode
;; example: add keys for forward and backward
;; (yyorg-bookmark-gen-keymap-minor-mode yyyy nil
;;    ("C-c C-c n" . 'forward-char)
;;    ("C-c C-c p" . 'backward-char))
(defmacro t-gen-keymap-minor-mode (keyname amap &rest keys-pair)
  "use `keyname' as minor-mode's name, k means yyob-k-mode, etc.
please use it after anything key binding you need have appeared
if amap is not nil, combine it with keymap created by this macro
amap must be nil or a symbol containing keymap"
  (declare (indent defun))
  (let* ((map-name (gensym))
	 (minor-mode-name (intern (concat "yyob-"
					  (symbol-name keyname)
					  "-mode")))
	 (keys-form (mapcar (lambda (x)
			      `(define-key ,map-name (kbd ,(car x)) ,(cdr x)))
			    keys-pair)))
    `(let ((,map-name (make-sparse-keymap)))
       ,@keys-form
       ,(when amap `(setq ,map-name
			  (make-composed-keymap (list ,map-name ,amap))))
       (define-minor-mode ,minor-mode-name
	 ,(concat "yyorg-bookamrk's buffer local minor mode keymap: "
		  (symbol-name minor-mode-name))
	 :keymap ,map-name)
       (,minor-mode-name 1))))

(provide 'yyorg-bookmark)

;;; yyorg-bookmark.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "yyorg-bookmark-"))
;; End:
