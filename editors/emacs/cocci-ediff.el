;;; cocci-ediff.el --- ediff support for semantic patches

;; Copyright (C) 2006-2007 The Cocci Gang

;; Emacs Lisp Archive Entry
;; Author: Rene Rydhof Hansen <rrhansen@diku.dk>
;;         Padioleau Yoann <padator@wanadoo.fr> (modifying the ediff layout)
;; Version: 0.1
;; Keywords: coccinelle patch refactoring program transformation
;; URL: http://www.emn.fr/x-info/coccinelle/


;;; Ediff and dired support 

;; You must modify the variables `cocci-spatch-path', 
;; `cocci-isofile-path', and `cocci-spatch-args' according to your setup.

;; Once it is installed you use it by loading a .cocci file (called the
;; SP), e.g., 'coccinelle/tests/rule17.cocci'. From the buffer containing
;; the SP you then press `C-cd' (or `M-x cocci-directory') and specify
;; the directory where your target C files are located, e.g.,
;; 'coccinelle/tests/rule17/', pick one of the listed C files (place the
;; cursor on it) and press `E' (or `M-x cocci-ediff-merge'). This will
;; then run spatch and apply the SP to the chosen C file; when spatch
;; finishes Ediff will start in a merge session, displaying the original
;; C file, the spatch'ed file and the result of merging those two. You
;; can now use Ediff for merging as usual. When you quit Ediff you will
;; be asked whether or not to replace the original file with the result
;; of the merge.


(require 'dired)
(require 'dired-x)
(require 'ediff)

(require 'cocci-mode) 

;--------------------------------------------------
; Defaults
;--------------------------------------------------

(defvar cocci-spatch-path "~/coccinelle/spatch")
(defvar cocci-isofile-path "~/coccinelle/standard.iso")
(defvar cocci-spatch-args 
  "-no_show_ctl_text -no_show_transinfo -no_parse_error_msg -no_show_misc")
(defvar cocci-spatch-default-output "/tmp/output.c")
(defvar cocci-save-merge-result nil
  "Determines if the result of merging files should be saved.")


;--------------------------------------------------
; Key map for Dired under Cocci
;--------------------------------------------------
(defvar cocci-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "N" 'dired-next-marked-file)
    (define-key map "P" 'dired-prev-marked-file)
    (define-key map "c" 'cocci-dired-compile-makeok)
    (define-key map "\C-c" 'cocci-dired-compile-makeok)
    (define-key map "\C-r" 'cocci-dired-run-spatch)
    (define-key map "r" 'cocci-dired-compile-spatch)
    (define-key map "v" 'cocci-dired-view-file)
    (define-key map "V" 'cocci-dired-view-corresponding-file)
    (define-key map [(control return)]  'cocci-dired-view-corresponding-file)
    (define-key map "*c" 'cocci-dired-mark-c-files)
    (define-key map "*C" 'cocci-dired-mark-cocci-files)
    (define-key map "*o" 'cocci-dired-mark-ok-files)
    (define-key map "*r" 'cocci-dired-mark-expected-files)
    (define-key map "*f" 'cocci-dired-mark-failed-files)
    (define-key map "T" 'cocci-dired-toggle-terse-mode)
    (define-key map "E" 'cocci-ediff-merge)
    (define-key map "D" 'cocci-ediff-diff)
    map)
  "Keymap used for cocci bindings in `dired-mode'.")


;--------------------------------------------------
; Internal Variables
;-------------------------------------------------- 

(defvar cocci-current-cocci nil
  "The current cocci-file")

(defvar cocci-spatch-output nil
  "The buffer for spatch output")

(defvar cocci-current-cocci-buffer nil
  "The current cocci-filebuffer")

;--------------------------------------------------
; Misc helpers
;--------------------------------------------------

(defun get-spatch-output-buffer ()
  (if (buffer-live-p cocci-spatch-output)
      cocci-spatch-output
    (setq cocci-spatch-output (generate-new-buffer "*Spatch Output*"))))


;--------------------------------------------------
; Shell Commands
;--------------------------------------------------
(defun cocci-spatch-cmd (sp)
  "Assembles command line for spatch"
  (concat cocci-spatch-path 
	  " -iso_file " cocci-isofile-path 
;	  " -compare_with_expected"
	  " -cocci_file " sp 
	  " " cocci-spatch-args))

(defun cocci-makeok-cmd (sp)
  "Assembles command line for make ok"
  (concat "make "
	  " ISOFILE=\"-iso_file " cocci-isofile-path "\""
	  " SP=" sp 
	  " ARGS=\"" cocci-spatch-args "\""))


;--------------------------------------------------
; Misc.
;--------------------------------------------------
(defun cocci-convert-ends (from to files)
  "Convert files (in files) from ending in from to ending to"
  (mapcar (lambda (f) 
	    (if (string-match (concat from "$") f)
		(replace-match to t t f)
	      f))
	  files))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cocci-ediff-windows)
(defvar cocci-ediff-result)
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)

(defvar ediff-show-sp t)

(defun cocci-merge-files (orig-file new-file &optional name-A name-B)
  "Invoke ediff to review application of SP and manually perform merge."
  (interactive)
  (let* ((found nil))
    (save-excursion

      ;; Set-up
      (let ((config (current-window-configuration))
            (ediff-default-variant 'default-B)
	    (ediff-keep-variants t))

        ;; Fire up ediff.

        (set-buffer (ediff-merge-files orig-file new-file))


        ;; Ediff is now set up, and we are in the control buffer.
        ;; Do a few further adjustments and take precautions for exit.

	(make-local-variable 'cocci-ediff-orig)
	(setq cocci-ediff-orig orig-file)

        (make-local-variable 'cocci-ediff-windows)
        (setq cocci-ediff-windows config)
;        (make-local-variable 'cocci-ediff-result)
;        (setq cocci-ediff-result result-buffer)
        (make-local-variable 'ediff-quit-hook)
        (setq ediff-quit-hook
              (lambda ()
		(let ((buffer-A ediff-buffer-A)
		      (buffer-B ediff-buffer-B)
		      (buffer-C ediff-buffer-C)
;		      (result cocci-ediff-result)
		      (windows cocci-ediff-windows)
		      (original cocci-ediff-orig))
		  (ediff-cleanup-mess)
;		  (ediff-janitor)
;		  (set-buffer result)
;		  (erase-buffer)
;		  (insert-buffer buffer-C)
		  (kill-buffer buffer-A)
		  (kill-buffer buffer-B)
		  (when cocci-save-merge-result
		    (switch-to-buffer buffer-C)
		    (delete-other-windows)
		    (ediff-write-merge-buffer-and-maybe-kill buffer-C original))
;		  (kill-buffer buffer-C)
		  (set-window-configuration windows)
		  (message "Merge resolved; you may save the buffer"))))
        (message "Please resolve merge now; exit ediff when done")
        nil))))


; pad's code
; merge between ediff-setup-windows-plain-compare and 
; ediff-setup-windows-plain from 'ediff-wind.el'
(defun cocci-ediff-setup-windows-plain (buf-A buf-B buf-C control-buffer)
  (ediff-with-current-buffer control-buffer
    (setq ediff-multiframe nil))

  (ediff-destroy-control-frame control-buffer)
  (let ((window-min-height 1)
	split-window-function wind-width-or-height
	three-way-comparison
	wind-A-start wind-B-start wind-A wind-B wind-C)
    (ediff-with-current-buffer control-buffer
      (setq wind-A-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'A ediff-narrow-bounds))
	    wind-B-start (ediff-overlay-start
			  (ediff-get-value-according-to-buffer-type
			   'B  ediff-narrow-bounds))
	    ;; this lets us have local versions of ediff-split-window-function
	    split-window-function ediff-split-window-function
	    three-way-comparison ediff-3way-comparison-job))
    (delete-other-windows)
    (split-window-vertically)
    (ediff-select-lowest-window)

    ;NEW
    (setq lowest-wind (selected-window))

    (ediff-setup-control-buffer control-buffer)
    
    ;; go to the upper window and split it betw A, B, and possibly C
    (other-window 1)

    ;NEW
    (split-window-vertically)
    (setq this-wind (selected-window))
    (other-window 1)
    (switch-to-buffer cocci-current-cocci-buffer)
    (select-window this-wind)

    (switch-to-buffer buf-A)
    (setq wind-A (selected-window))

    (if three-way-comparison
	(setq wind-width-or-height
	      (/ (if (eq split-window-function 'split-window-vertically)
		     (window-height wind-A)
		   (window-width wind-A))
		 3)))
    
    ;; XEmacs used to have a lot of trouble with display
    ;; It did't set things right unless we told it to sit still
    ;; 19.12 seems ok.
    ;;(if ediff-xemacs-p (sit-for 0))
    
;    (funcall split-window-function wind-width-or-height)
    (split-window-horizontally)
		  
    (if (eq (selected-window) wind-A)
	(other-window 1))
    (switch-to-buffer buf-B)
    (setq wind-B (selected-window))
	  
    (if three-way-comparison
	(progn
	  (funcall split-window-function) ; equally
	  (if (eq (selected-window) wind-B)
	      (other-window 1))
	  (switch-to-buffer buf-C)
	  (setq wind-C (selected-window))))
	  
    (ediff-with-current-buffer control-buffer
      (setq ediff-window-A wind-A
	    ediff-window-B wind-B
	    ediff-window-C wind-C))
    
    ;; It is unlikely that we will want to implement 3way window comparison.
    ;; So, only buffers A and B are used here.
    (if ediff-windows-job
	(progn
	  (set-window-start wind-A wind-A-start)
	  (set-window-start wind-B wind-B-start)))
  
    (ediff-select-lowest-window)
    (ediff-setup-control-buffer control-buffer)
    ))



(defvar old-ediff-setup-function ediff-window-setup-function)

; pad's code, almost copy paste of rene's cocci-merge-files
(defun cocci-diff-files (orig-file new-file &optional name-A name-B)
  "Invoke ediff to review application of SP and manually perform merge."
  (interactive)
  (let* ((found nil))
    (save-excursion

      ;; Set-up
      (let ((config (current-window-configuration))
            (ediff-default-variant 'default-B)
	    (ediff-keep-variants t)
            )

        ;; Fire up ediff.



        ;NEW, use ediff-files
        (setq ediff-window-setup-function 'cocci-ediff-setup-windows-plain)

        (set-buffer (ediff-files orig-file new-file))


        ;; Ediff is now set up, and we are in the control buffer.
        ;; Do a few further adjustments and take precautions for exit.

	(make-local-variable 'cocci-ediff-orig)
	(setq cocci-ediff-orig orig-file)

        (make-local-variable 'cocci-ediff-windows)
        (setq cocci-ediff-windows config)
;        (make-local-variable 'cocci-ediff-result)
;        (setq cocci-ediff-result result-buffer)
        (make-local-variable 'ediff-quit-hook)
        (setq ediff-quit-hook
              (lambda ()
		(let ((buffer-A ediff-buffer-A)
		      (buffer-B ediff-buffer-B)
		      (buffer-C ediff-buffer-C)
;		      (result cocci-ediff-result)
		      (windows cocci-ediff-windows)
		      (original cocci-ediff-orig))
		  (ediff-cleanup-mess)
;		  (ediff-janitor)
;		  (set-buffer result)
;		  (erase-buffer)
;		  (insert-buffer buffer-C)
		  (kill-buffer buffer-A)
		  (kill-buffer buffer-B)

                  (setq ediff-window-setup-function old-ediff-setup-function)

		  (when cocci-save-merge-result
		    (switch-to-buffer buffer-C)
		    (delete-other-windows)
		    (ediff-write-merge-buffer-and-maybe-kill buffer-C original))
;		  (kill-buffer buffer-C)
		  (set-window-configuration windows)
		  (message "Merge resolved; you may save the buffer"))))
        (message "Please resolve merge now; exit ediff when done")
        nil))))


;----------------------------------------------------------------------
; Executing "make" and "spatch" commands
;----------------------------------------------------------------------

(defun cocci-dired-compile-makeok (arg)
  "Compiles the marked files in cocci/dired mode. With prefix arg no file
names are substituted (useful for Makefiles)."
  (interactive "P")
  (if arg 
      (compile (read-from-minibuffer "Compile command: "
				     (eval compile-command) nil nil
				     '(compile-history . 1)))
    (let* ((file-list (cocci-convert-ends ".c" ".ok" (dired-get-marked-files t)))
	   (command (dired-mark-read-string
		     "Make targets %s with: " 
		     (cocci-makeok-cmd cocci-current-cocci)
		     'compile nil file-list)))
      (compile (concat command " " (mapconcat 'identity file-list " "))))))

(defun cocci-dired-compile-spatch (&optional arg)
  "Runs spatch on current file. Non-nil optional arg to specify command."
  (interactive "P")
  (if arg 
      (compile (read-from-minibuffer "Command: "
				     (eval compile-command) nil nil
				     '(compile-history . 1)))
    (let ((file (dired-get-filename t))
	  (command (cocci-spatch-cmd cocci-current-cocci)))
      (compile (concat command " " file)))))

(defun cocci-apply-spatch (file &optional sp-file out-buf)
  "Applies the current SP to FILE."
  (interactive)
  (let ((cmd (concat (cocci-spatch-cmd (or sp-file cocci-current-cocci)))))
    (shell-command (concat cmd " " file) out-buf)))

(defun cocci-ediff-merge (&optional res-file)
  "Use EDiff to review and apply semantic patch."
  (interactive)
  (let ((file (dired-get-filename t))
	(out-buf (get-spatch-output-buffer)))
    (message "Applying SP '%s' to file '%s'..." 
	     (file-name-nondirectory cocci-current-cocci) 
	     (file-name-nondirectory file))
    (cocci-apply-spatch file cocci-current-cocci out-buf)
    (message "Applying SP '%s' to file '%s'... done." 
	     (file-name-nondirectory cocci-current-cocci) 
	     (file-name-nondirectory file))
;    (ediff-merge-files file (or res-file cocci-spatch-default-output))
    (cocci-merge-files file (or res-file cocci-spatch-default-output))))


(defun cocci-ediff-diff (&optional res-file)
  "Use EDiff to review and apply semantic patch."
  (interactive)
  (let ((file (dired-get-filename t))
	(out-buf (get-spatch-output-buffer)))
    (message "Applying SP '%s' to file '%s'..." 
	     (file-name-nondirectory cocci-current-cocci) 
	     (file-name-nondirectory file))
    (cocci-apply-spatch file cocci-current-cocci out-buf)
    (message "Applying SP '%s' to file '%s'... done." 
	     (file-name-nondirectory cocci-current-cocci) 
	     (file-name-nondirectory file))
;    (ediff-merge-files file (or res-file cocci-spatch-default-output))
    (cocci-diff-files file (or res-file cocci-spatch-default-output))))

(defun cocci-dired-view-file ()
  "In cocci dired, visit the file or directory named on this line 
using diff-mode."
  (interactive)
  (let ((file-name (file-name-sans-versions (dired-get-filename) t))
	;; bind it so that the command works on directories too,
	;; independent of the user's setting
	(find-file-run-dired t))
    (if (file-exists-p file-name)
	(progn
	  (find-file file-name)
	  (diff-mode))
      (if (file-symlink-p file-name)
	  (error "File is a symlink to a nonexistent target")
	(error "File no longer exists; type `g' to update Dired buffer")))))

(defun cocci-dired-view-corresponding-file ()
  "In cocci dired, visit the file or directory named on this line 
using diff-mode."
  (interactive)
  (let* ((file-name 
	  (cocci-corresponding-file 
	   (file-name-sans-versions (dired-get-filename) t)))
	 (find-file-run-dired t))
    (if (file-exists-p file-name)
	(progn
	  (find-file file-name)
	  (diff-mode))
      (if (file-symlink-p file-name)
	  (error "File is a symlink to a nonexistent target")
	(error "File no longer exists; type `g' to update Dired buffer")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cocci-dired-run-spatch (&optional spfile)
  "In a dired buffer, runs spatch on marked files with source buffer as SP file"
  (interactive)
  (when (not spfile) (setq spfile cocci-current-cocci))
  (dired-do-shell-command (concat (cocci-spatch-cmd spfile) " ?")
			  current-prefix-arg
			  (dired-get-marked-files t current-prefix-arg)))

(defun cocci-dired-run-makeok (spfile)
  "In a dired buffer, runs 'make ok' on marked files with 
source buffer as SP file"
  (interactive)
  (let ((files (dired-get-marked-files t current-prefix-arg)))
    (dired-do-shell-command (concat (cocci-makeok-cmd spfile) " ?") 
			    current-prefix-arg 
			    (cocci-convert-ends ".c" ".ok" files))))

(defun cocci-run-makeok (spfile)
  "Run 'make ok' in current directory with source buffer as SP file"
  (interactive)
  (compile (cocci-makeok-cmd (buffer-file-name))))



;--------------------------------------------------
; Marking files in Dired under Cocci
;--------------------------------------------------

(defun cocci-dired-mark-c-files ()
  (interactive)
  (dired-mark-extension ".c"))

(defun cocci-dired-mark-cocci-files ()
  (interactive)
  (dired-mark-extension ".cocci"))

(defun cocci-dired-mark-ok-files ()
  "Mark all .ok files. In terse mode mark all .c files that have a 
corresponding .ok file"
  (interactive)
  (if (not cocci-dired-terse-mode)
      (dired-mark-extension ".ok")
    (dired-mark-if (let ((f (dired-get-filename nil t)))
		     (and f
			  (file-exists-p (cocci-file f 'ok))
			  (not (file-directory-p f))))
		   "source file(s) with OK result file")))

(defun cocci-dired-mark-failed-files ()
  "Mark all .failed files. In terse mode mark all .c files that have a 
corresponding .failed file"
  (interactive)
  (if (not cocci-dired-terse-mode)
      (dired-mark-extension ".failed")
    (dired-mark-if (let ((f (dired-get-filename nil t)))
		     (and f
			  (file-exists-p (cocci-file f 'fail))
			  (not (file-directory-p f))))
		   "source file(s) with FAILED result file")))

(defun cocci-dired-mark-expected-files ()
  "Mark all .res files. In terse mode mark all .c files that have a 
corresponding .res file"
  (interactive)
  (if (not cocci-dired-terse-mode)
      (dired-mark-extension ".res")
    (dired-mark-if (let ((f (dired-get-filename nil t)))
		     (and f
			  (file-exists-p (cocci-file f 'expected))
			  (not (file-directory-p f))))
		   "source file(s) with EXPECTED result file")))


;;; One shot - for counting in README's
;;; FIXME: remove
(defun cocci-count-status ()
  (interactive)
  (let (res)
    (save-excursion
      (beginning-of-buffer)
      (let ((ok (count-matches "\\[status\\] ok"))
	    (sok (count-matches "\\[status\\] spatch-ok"))
	    (fail (count-matches "\\[status\\] fail"))
	    (wrong (count-matches "\\[status\\] wrong")))
	(setq res (replace-regexp-in-string 
		 " occurrences" 
		 "" 
		 (concat "[ok: " ok "; spatch-ok: " sok 
			 "; fail: " fail "; wrong: " wrong "]")))))
    (insert res)))


;----------------------------------------------------------------------
; Cocci Dired - Adapted from VC Dired
;----------------------------------------------------------------------

(defvar cocci-dired-listing-switches "-al"
  "*Switches passed to `ls' for vc-dired.  MUST contain the `l' option.")

(defvar cocci-dired-terse-display t
  "*If non-nil, show only source (.c) files in Cocci Dired.")

(defvar cocci-dired-recurse t
  "*If non-nil, show directory trees recursively in VC Dired.")

(defvar cocci-directory-exclusion-list '("SCCS" "RCS" "CVS")
  "*List of directory names to be ignored when walking directory trees.")

(defvar cocci-dired-switches)
(defvar cocci-dired-terse-mode)
(defvar cocci-dired-mode nil)
(make-variable-buffer-local 'cocci-dired-mode)

;;; based on vc-dired-mode
(define-derived-mode cocci-dired-mode dired-mode "Dired under Cocci"
  "The major mode used in Cocci directory buffers.

It works like Dired, with the current Cocci state of each file being 
indicated in the place of the file's link count, owner, group and size.
Subdirectories are also listed, and you may insert them into the buffer 
as desired, like in Dired."
  ;; define-derived-mode does it for us in Emacs-21, but not in Emacs-20.
  ;; We do it here because dired might not be loaded yet
  ;; when vc-dired-mode-map is initialized.
  (set-keymap-parent cocci-dired-mode-map dired-mode-map)
  (make-local-hook 'dired-after-readin-hook)
  (add-hook 'dired-after-readin-hook 'cocci-dired-hook nil t)
  ;; The following is slightly modified from dired.el,
  ;; because file lines look a bit different in vc-dired-mode.
  (set (make-local-variable 'dired-move-to-filename-regexp)
       (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
	      ;; In some locales, month abbreviations are as short as 2 letters,
	      ;; and they can be followed by ".".
	      (month (concat l l "+\\.?"))
	      (s " ")
	      (yyyy "[0-9][0-9][0-9][0-9]")
	      (dd "[ 0-3][0-9]")
	      (HH:MM "[ 0-2][0-9]:[0-5][0-9]")
	      (seconds "[0-6][0-9]\\([.,][0-9]+\\)?")
	      (zone "[-+][0-2][0-9][0-5][0-9]")
	      (iso-mm-dd "[01][0-9]-[0-3][0-9]")
	      (iso-time (concat HH:MM "\\(:" seconds "\\( ?" zone "\\)?\\)?"))
	      (iso (concat "\\(\\(" yyyy "-\\)?" iso-mm-dd "[ T]" iso-time
			   "\\|" yyyy "-" iso-mm-dd "\\)"))
	      (western (concat "\\(" month s "+" dd "\\|" dd "\\.?" s month "\\)"
			       s "+"
			       "\\(" HH:MM "\\|" yyyy "\\)"))
	      (western-comma (concat month s "+" dd "," s "+" yyyy))
	      ;; Japanese MS-Windows ls-lisp has one-digit months, and
	      ;; omits the Kanji characters after month and day-of-month.
	      (mm "[ 0-1]?[0-9]")
	      (japanese
	       (concat mm l "?" s dd l "?" s "+"
		       "\\(" HH:MM "\\|" yyyy l "?" "\\)")))
	 ;; the .* below ensures that we find the last match on a line
	 (concat ".*" s
		 "\\(" western "\\|" western-comma "\\|" japanese "\\|" iso "\\)"
		 s "+")))
  (and (boundp 'cocci-dired-switches)
       cocci-dired-switches
       (set (make-local-variable 'dired-actual-switches)
            cocci-dired-switches))
  (set (make-local-variable 'cocci-dired-terse-mode) cocci-dired-terse-display)
  (setq cocci-dired-mode t))

(defun cocci-dired-toggle-terse-mode ()
  "Toggle terse display in Cocci Dired."
  (interactive)
  (if (not cocci-dired-mode)
      nil
    (setq cocci-dired-terse-mode (not cocci-dired-terse-mode))
    (if cocci-dired-terse-mode
        (cocci-dired-hook)
      (revert-buffer))))

(defun cocci-convert-file-ext (file ext)
  (concat (file-name-sans-extension file) ext))

(defvar cocci-file-types
  '((cocci . ".cocci") (source . ".c") (ok . ".ok") 
    (fail . ".failed") (expected . ".res"))
"Alist of file name extensions used by Cocci.")

(defun cocci-file (file type)
  (let ((ext (cdr (assq type cocci-file-types))))
    (if ext
	(cocci-convert-file-ext file ext)
      (error "cocci-file: requested unknown file type (%s)" type))))

(defun cocci-file-type (file)
  (car (rassoc (file-name-extension file t) cocci-file-types)))

(defun cocci-result-type-p (type) (or (eq type 'ok) (eq type 'fail)))

(defun cocci-corresponding-file (file)
  (let ((ftype (cocci-file-type file)))
    (cond
     ((or (eq ftype 'ok) (eq ftype 'fail)) (cocci-file file 'source))
     ((eq ftype 'source)
      (let ((status (cocci-result-status file)))
	(cond
	 ((eq status 'cocci-ok) (cocci-file file 'ok))
	 ((eq status 'cocci-fail) (cocci-file file 'fail))
	 (t (error "No corresponding result file for %s" file)))))
     (t (error "No corresponding file for %s" file)))))

(defun cocci-result-status (file)
  "For a source file return the Coccinelle result status (if any)."
  (let ((ok   (file-exists-p (cocci-file file 'ok)))
	(fail (file-exists-p (cocci-file file 'fail)))
	(res  (file-exists-p (cocci-file file 'expected))))
    (cond
     ((and ok fail) 'cocci-conflict)  ; old .ok/.failed files lying around?
     (ok            'cocci-ok)        ; found a .ok file
     (fail          'cocci-fail)      ; found a .fail file
     (res           'cocci-update)    ; found an expected result but no result
     (t             'cocci-unknown)))) ; file is not under cocci "control"

(defun cocci-stale-result-file (file &optional src-file)
  (when (not src-file) (setq src-file (cocci-file file 'source)))
  (and (file-exists-p file)
       (or (file-newer-than-file-p cocci-current-cocci file)
	   (file-newer-than-file-p src-file file))))

;; Trying to abstract away from files
(defun cocci-stale-result (file &optional result)
  "Determine if the result is stale."
  (let ((src-file (cocci-file file 'source)))
    (if result
	(cocci-stale-result-file (cocci-file file result) src-file)
      (or (cocci-stale-result-file (cocci-file file 'ok) src-file)
	  (cocci-stale-result-file (cocci-file file 'fail) src-file)))))

(defun cocci-dired-state-info (file)
  (cond
   ; a source (.c) file
   ((eq (cocci-file-type file) 'source)
    (let ((result)
	  (status (cocci-result-status file)))
      (setq result
	     (cond
	      ((eq status 'cocci-ok)
	       (if (cocci-stale-result file 'ok)
		   "(ok?)"
		 "(ok)"))
	      ((eq status 'cocci-fail)
	       (if (cocci-stale-result file 'fail)
		   "(fail?)"
		 "(fail)"))
	      ((eq status 'cocci-conflict) "(ok/fail)")
	      ((eq status 'cocci-update)   "(update)")
	      ((eq status 'cocci-unknown)  "(unknown)")
	      (t nil)))
      (substring (concat result "        ") 0 10)))
   ))

(defun cocci-dired-reformat-line (x)
  "Reformat a directory-listing line.
Replace various columns with version control information.
This code, like dired, assumes UNIX -l format."
  (beginning-of-line)
  (let ((pos (point)) limit perm date-and-file)
    (end-of-line)
    (setq limit (point))
    (goto-char pos)
    (when
        (or
         (re-search-forward  ;; owner and group
          "^\\(..[drwxlts-]+ \\) *[0-9]+ [^ ]+ +[^ ]+ +[0-9]+\\( .*\\)"
          limit t)
         (re-search-forward  ;; only owner displayed
          "^\\(..[drwxlts-]+ \\) *[0-9]+ [^ ]+ +[0-9]+\\( .*\\)"
	  limit t)
         (re-search-forward  ;; OS/2 -l format, no links, owner, group
          "^\\(..[drwxlts-]+ \\) *[0-9]+\\( .*\\)"
          limit t))
      (setq perm          (match-string 1)
	    date-and-file (match-string 2))
      (setq x (substring (concat x "          ") 0 10))
      (replace-match (concat perm x date-and-file)))))

(defun cocci-dired-hook ()
  "Reformat the listing according to version control.
Called by dired after any portion of a cocci-dired buffer has been read in."
  (message "Getting status information... ")
  (let (subdir filename (buffer-read-only nil) cvs-dir)
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ;; subdir header line
       ((setq subdir (dired-get-subdir))
	;; if the backend supports it, get the state
	;; of all files in this directory at once
;	(let ((backend (vc-responsible-backend subdir)))
;	  (if (vc-find-backend-function backend 'dir-state)
;	      (vc-call-backend backend 'dir-state subdir)))
        (forward-line 1)
        ;; erase (but don't remove) the "total" line
	(delete-region (point) (line-end-position))
	;; Ugly hack to display the current cocci file.
	;; Needed because of hardcoded dired regexps
	(when cocci-current-cocci
	  (insert 
	   (concat
	    (propertize " " 'display '((margin nil) "  Current cocci file: "))
	    (propertize " " 
			'display 
			`((margin nil) 
			  ,(file-name-nondirectory cocci-current-cocci))))))
	(beginning-of-line)
	(forward-line 1))
       ;; file line
       ((setq filename (dired-get-filename nil t))
        (cond
         ;; subdir
         ((file-directory-p filename)
          (cond
           ((member (file-name-nondirectory filename)
                    cocci-directory-exclusion-list)
            (let ((pos (point)))
              (dired-kill-tree filename)
              (goto-char pos)
              (dired-kill-line)))
           (cocci-dired-terse-mode
            ;; Don't show directories in terse mode.  Don't use
            ;; dired-kill-line to remove it, because in recursive listings,
            ;; that would remove the directory contents as well.
            (delete-region (line-beginning-position)
                           (progn (forward-line 1) (point))))
           ((string-match "\\`\\.\\.?\\'" (file-name-nondirectory filename))
            (dired-kill-line))
           (t
            (cocci-dired-reformat-line nil)
            (forward-line 1))))
         ;; ordinary file
	 ;; show only .c in terse mode
         ((or (and (eq (cocci-file-type filename) 'source)
		   (equal (file-name-sans-versions filename) filename))
	      (not (and cocci-dired-terse-mode)))
          (cocci-dired-reformat-line (cocci-dired-state-info filename))
          (forward-line 1))
         (t
          (dired-kill-line))))
       ;; any other line
       (t (forward-line 1))))
    (cocci-dired-purge))
  (message "Getting status information... done"))

(defun cocci-dired-purge ()
  "Remove empty subdirs."
  (let (subdir)
    (goto-char (point-min))
    (while (setq subdir (dired-get-subdir))
      (forward-line 2)
      (if (dired-get-filename nil t)
          (if (not (dired-next-subdir 1 t))
              (goto-char (point-max)))
        (forward-line -2)
        (if (not (string= (dired-current-directory) default-directory))
            (dired-do-kill-lines t "")
          ;; We cannot remove the top level directory.
          ;; Just make it look a little nicer.
          (forward-line 1)
          (kill-line)
          (if (not (dired-next-subdir 1 t))
              (goto-char (point-max))))))
    (goto-char (point-min))))

(defun cocci-dired-buffers-for-dir (dir)
  "Return a list of all cocci-dired buffers that currently display DIR."
  (let (result)
    ;; Check whether dired is loaded.
    (when (fboundp 'dired-buffers-for-dir)
      (mapcar (lambda (buffer)
		(with-current-buffer buffer
		  (if cocci-dired-mode
		      (setq result (append result (list buffer))))))
	      (dired-buffers-for-dir dir)))
    result))

(defun cocci-dired-resynch-file (file)
  "Update the entries for FILE in any Cocci Dired buffers that list it."
  (let ((buffers (cocci-dired-buffers-for-dir (file-name-directory file))))
    (when buffers
      (mapcar (lambda (buffer)
		(with-current-buffer buffer
		  (if (dired-goto-file file)
		      ;; bind vc-dired-terse-mode to nil so that
		      ;; files won't vanish when they are checked in
		      (let ((cocci-dired-terse-mode nil))
			(dired-do-redisplay 1)))))
	      buffers))))

(defun cocci-directory (dir read-switches)
  "Create a buffer in Cocci Dired Mode for directory DIR.

With prefix arg READ-SWITCHES, specify a value to override
`dired-listing-switches' when generating the listing."
  (interactive "DDired under Cocci (directory): \nP")
  (let ((cocci-dired-switches (concat cocci-dired-listing-switches
                                   (if cocci-dired-recurse "R" ""))))
    (if read-switches
        (setq cocci-dired-switches
              (read-string "Dired listing switches: "
                           cocci-dired-switches)))
    (require 'dired)
    (require 'dired-aux)
    (switch-to-buffer
     (dired-internal-noselect (expand-file-name (file-name-as-directory dir))
                              cocci-dired-switches
                              'cocci-dired-mode))))



;--------------------------------------------------
; Hook
;--------------------------------------------------

(define-key cocci-mode-map  "\C-cd" 'cocci-directory)

(add-hook 'cocci-mode-hook 
          (lambda () 
            (setq cocci-current-cocci (buffer-file-name))
            (setq cocci-current-cocci-buffer (current-buffer))
            (setq compile-command (cocci-makeok-cmd cocci-current-cocci))))


(provide 'cocci-ediff)