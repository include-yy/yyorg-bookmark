;;; yy-org-bookmark.el yy's bookmark manage tool -*- lexical-binding: t -*-

(require 'cl-lib)

(require 'org)
(require 'org-capture)
(require 'org-protocol)
(require 'org-attach)

(defvar t-headline "YYOB-MANAGEMENT"
  "enchant file's 1st headline's name")
(defvar t-enchant-file (expand-file-name "./enchant.org")
  "default enchant file for file variable initialize")
(defvar t-global-counter-name "YYOB-COUNTER"
  "name of enchant file's global counter")
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
  "used in capture template"
  (let* ((filename (t--template-filename key))
	 (buf (get-file-buffer filename)))
    (with-current-buffer buf
      (t-control-global-counter op))))

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

(provide 'yyorg-bookmark)

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "yyorg-bookmark-"))
;; End:
