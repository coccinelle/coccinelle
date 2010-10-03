;;; cocci.el --- a major mode for editing semantic patches

;; Copyright (C) 2010      Nicolas Palix <npalix@diku.dk>
;; Copyright (C) 2006-2007 Yoann Padioleau

;; Please imagine a long and boring gnu-style copyright notice
;; appearing just here.


;; Emacs Lisp Archive Entry
;; Author: Padioleau Yoann <yoann.padioleau@gmail.com>,
;; Version: 0.2
;; Keywords: coccinelle patch refactoring program transformation
;; URL: http://coccinelle.lip6.fr/


;;; Usage

;; Copy this file in your ~/.emacs.d directory
;;
;; Add the following lines to your ~/.emacs or equivalent:
;;  (load "~/.emacs.d/cocci.el")
;;  (setq auto-mode-alist
;;          (cons '("\\.cocci$" . cocci-mode) auto-mode-alist))
;;  (autoload 'cocci-mode "cocci"
;;          "Major mode for editing cocci code." t)
;;
;; You can also use cocci-mode to edit the files containing the
;; isomorphisms with:
;;  (setq auto-mode-alist
;;           (cons '("\\.iso$" . cocci-mode) auto-mode-alist))
;;

;;; History

;; 2010-04-02 Nico: Fix 'script' with 'depends on'. Add 'when forall', 'when any'
;; 2010-02-01 Nico: Add support for 'disable', 'using', scripting, 'virtual' rules
;; 2009-11-05 Nico: Cleanups, Change shortcut % to C-M-% (% is used in Python rule)
;; Some cleanups done by Rene Rydhof Hansen

;;; Utilities

(defun join-sep (sep xs)
  (mapconcat 'identity xs sep))


;;; Variables

(defvar cocci-menu)


;; new (color) faces

(defface cocci-number-face
  '((((background light)) (:foreground "black"))
    (((background dark)) (:foreground "yellow3")))
  "Used for Cocci numbers")

(defface cocci-punctuation-face
  '((((background light)) (:foreground "black"))
    (((background dark)) (:foreground "cyan")))
  "Used for punctuation")

(defface cocci-problem-face
  '((((background light)) (:background "deep pink"))
    (((background dark)) (:background "deep pink")))
  "Highlighting potential problems")

(defface cocci-special-face
  '((((background light)) (:foreground "blue"))
    (((background dark)) (:foreground "red")))
  "")

(defface cocci-rulename-face
  '((((background light)) (:foreground "DarkSlateGray"))
    (((background dark)) (:foreground "DarkSlateGray4")))
  "Highlighting the rule names")

(defface cocci-minus-face
  '((((background light)) (:foreground "red"))
    (((background dark)) (:foreground "SeaGreen3")))
  "Highlighting lines to be removed")

(defface cocci-plus-face
  '((((background light)) (:foreground "dark green"))
    (((background dark)) (:foreground "salmon")))
  "Highlighting lines to be added")

(defface cocci-match-face
  '((((background light)) (:foreground "violet red"))
    (((background dark)) (:foreground "purple")))
  "Highlighting lines to be matched (sgrep)")

(defface cocci-script-face
  '((((background light)) (:foreground "red"))
    (((background dark)) (:foreground "SeaGreen3")))
  "Highlighting script language name")


;; can look in lexer_cocci.mll for new identifiers

(defconst cocci-c-keywords-list
  '("if" "else" "while" "do" "for" "return"
    "sizeof"
    "struct" "union"
    "static" "extern" "const" "volatile"
    "break" "continue"
    "switch" "case"
    ))

(defconst cocci-declaration-keywords-list
  '("identifier" "type" "parameter" "constant" "expression" "statement"
    "function" "local" "list"
    "fresh"
    "position"
    "idexpression"

    "context"

    "typedef"
    "declarer" "iterator"
    "pure"
    ;"error" "words"

    "char" "short" "int" "float" "double" "long"
    "void"
    "signed" "unsigned"
    ))

(defconst cocci-iso-keywords-list
  '("Expression" "Statement" "Type"
    "Declaration" "TopLevel" "ArgExpression"
    ))



(defconst c-preprocessor-directives-list
  '("define" "undef"
    "if" "ifdef" "elif" "else" "endif" "ifndef"
    "include"
    "error" "pragma"
    "file" "line"
    ))

(setq cocci-font-lock-keywords
 `(
   ; For virtual rule declarations
   ("^[ \t]*\\(virtual\\)\\b\\(.*\\)"
    (1 'cocci-special-face)
    (2 'cocci-rulename-face)
    )

   ; blink possible errors, when - or + is not in first column
   ("^[ \t]+[-+]" . 'cocci-problem-face)

   ; modifiers
   ("^\\??\\+.*" . 'cocci-plus-face)
   ("^\\??-.*"   . 'cocci-minus-face)

   ("^\\*.*"   . 'cocci-match-face)
   ;("^\\??\\+.*?//" . 'cocci-plus-face)
   ; ! \\+

   ; --- +++

   ; #cpp
   ("#\\(include\\) *\\(.*\\)"
    (1 'font-lock-builtin-face)
    (2 'font-lock-string-face)
    )

   ; comments
   ("//.*" . 'font-lock-comment-face)

   ; strings
   ("\"[^\"]*\"" . 'font-lock-string-face)

   ; rule header
   ("@[ \t]*@" . 'cocci-special-face)
   ; this rule may seems redundant with the following one, but
   ; without it, @@ int x; @@ would color the int x with rulename-face.
   ; by using this rule, we color the @@ and so prevent the
   ; next rule to be applied (cf font-lock semantic when have not the
   ; OVERRIDE flag).

   ("\\(@\\)\\(.*\\)\\(@\\)"
    (1 'cocci-special-face)
    (2 'cocci-rulename-face)
    (3 'cocci-special-face)
    )

   ("@.*\\b\\(extends\\|\\(depends[ \t]*on\\)\\)\\b.*@"
    (1 'cocci-special-face t))

   ("@.*\\b\\(disable\\)\\b.*@"
    (1 'cocci-special-face t))

   ("@.*\\b\\(using\\)\\b.*@"
    (1 'cocci-special-face t))

   ("@.*\\b\\(initialize\\)[ \t]*:[ \t]*\\(.*\\)[ \t]*@"
    (1 'cocci-special-face t)
    (2 'cocci-script-face t)
    )

   ("@.*\\b\\(script\\)[ \t]*:[ \t]*\\([^ ]*\\)[ \t]*.*@"
    (1 'cocci-special-face t)
    (2 'cocci-script-face t)
    )

   ("@.*\\b\\(finalize\\)[ \t]*:[ \t]*\\(.*\\)[ \t]*@"
    (1 'cocci-special-face t)
    (2 'cocci-script-face t)
    )

   ;old: does not work, not easy to handle the   rule1, rule2, rule3  list.
   ;   ("@[ \t]*\\(\\(\\w+\\)[ \t,]*\\)*[ \t]*@"
   ;   ("\\(@\\)[ \t]*\\(\\w+\\)[ \t]*\\(@\\)"
   ;   ("\\(@\\)[ \t]*\\(\\w+\\)[ \t]+\\(extends\\)[ \t]+\\(\\w+\\)[ \t]*\\(@\\)"
   ;   ("\\(@\\)[ \t]*\\(\\w+\\)[ \t]+\\(depends\\)[ \t]+\\(on\\)[ \t]+\\(\\(\\w+\\)[ ,\t]*\\)+\\(@\\)"


   ; inherited variable, fontifying rulename
   (,(concat "^"
      "\\b\\(" (regexp-opt cocci-declaration-keywords-list) "\\)\\b"
      ".*?\\(\\w+\\)\\.")
    (2 'cocci-rulename-face))

   ;rule1.T *a;
   ("^\\(\\w+\\)\\."
    (1 'cocci-rulename-face))

   ; just for pad, metavariables in maj
   ("\\b[A-Z][0-9]?\\b" . font-lock-variable-name-face)

   ; todo: do also for other variable, do as in font-lock.el
   ; with font-lock-match-c-style-declaration-item-and-skip-to-next

   ; special cocci operators
   ("\\.\\.\\." . 'font-lock-keyword-face)
   ("^[()|]" . 'font-lock-keyword-face)

   ; escaped version of cocci operators
   ("\\\\[()|]" . 'font-lock-keyword-face)

   ("\\bwhen[ \t]+!=" . 'font-lock-keyword-face)
   ("\\bWHEN[ \t]+!=" . 'font-lock-keyword-face)
   ("\\bwhen[ \t]+=" . 'font-lock-keyword-face)
   ("\\bWHEN[ \t]+=" . 'font-lock-keyword-face)
   ("\\bwhen[ \t]+forall" . 'font-lock-keyword-face)
   ("\\bWHEN[ \t]+forall" . 'font-lock-keyword-face)
   ("\\bwhen[ \t]+any" . 'font-lock-keyword-face)
   ("\\bWHEN[ \t]+any" . 'font-lock-keyword-face)

   ; used in iso files
   ("<=>" . 'font-lock-keyword-face)
   ("=>" . 'font-lock-keyword-face)

   (,(concat "\\b\\(" (regexp-opt cocci-iso-keywords-list) "\\)\\b") .
     'cocci-special-face)

   ("\\<[0-9]+\\>" . 'cocci-number-face)

   (,(join-sep "\\|"
        (list "(" ")" ";" "," "{" "}" "\\[" "\\]")) .  'cocci-punctuation-face)
   ; . ->   * + etc

   ; c keywords
   (,(concat "\\b\\(" (regexp-opt cocci-c-keywords-list) "\\)\\b") .
     'font-lock-keyword-face)

   ; cocci declaration keywords
   (,(concat "\\b\\(" (regexp-opt cocci-declaration-keywords-list) "\\)\\b") .
      'font-lock-type-face)

   ; cpp directives
   (,(concat "^#[ \t]*\\(" (regexp-opt c-preprocessor-directives-list)
	    "\\)\\>[ \t!]*\\(\\sw+\\)?")
    (1 'font-lock-builtin-face))

  ))
;  "Expressions to highlight in cocci-mode.")


;; define a mode-specific abbrev table for those who use such things
(defvar cocci-mode-abbrev-table nil
  "Abbrev table used while in cocci mode.")
(define-abbrev-table 'cocci-mode-abbrev-table nil)


(defvar cocci-mode-map nil
  "Keymap used in `cocci-mode'.")
(unless cocci-mode-map
  (setq cocci-mode-map (make-sparse-keymap))
  (define-key cocci-mode-map [(meta control *)] 'switch-between-cocci-c)
  (define-key cocci-mode-map "%" 'cocci-replace-modifiers)

  ;(define-key cocci-mode-map "\C-c" 'compile)
  )


(defvar cocci-mode-syntax-table nil
  "Syntax table used while in cocci mode.")
(unless cocci-mode-syntax-table
  (setq cocci-mode-syntax-table (make-syntax-table))

  ; _ is part of a word.
  (modify-syntax-entry ?\_ "w"  cocci-mode-syntax-table)

  ; change mode for ", bad interaction with font-lock
  (modify-syntax-entry ?\" "w"  cocci-mode-syntax-table)
  )


;;; Code

;; helper functions for the cocci programmer

(defun cocci-replace-modifiers (beg end str)
  "TODO"
  (interactive
   (let ((str (read-string "New modifier string (+, -, space): "
                           nil 'my-history)))
     (list (region-beginning) (region-end) str)))

  ;(interactive "rsNew modifier string (+, -, space): ")
  (replace-regexp "^[-+]?" str nil beg end)
  )

;Used internally while developping coccinelle.
;Allow to switch between the corresponding SP and C file.
;todo: handle the _verxxx naming convention.
(defun switch-between-cocci-c ()
  (interactive)
  (let ((target
         (cond ((string-match ".c$" (buffer-name))
                (replace-match ".cocci" t t (buffer-name)))
               ((string-match ".cocci$" (buffer-name))
                (replace-match ".c" t t (buffer-name)))
               (t
                "none"))))
    (if (get-buffer target)
	(switch-to-buffer target)
      (find-file
       (read-file-name "file: " nil nil t target)))))

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [(meta control *)] 'switch-between-cocci-c))
  )



(defvar cocci-mode-hook nil
  "Hook called by  `cocci-mode'")

;;;###autoload
(defun cocci-mode ()
  "Major mode for editing cocci code.
Special commands: \\{cocci-mode-map}
Turning on cocci-mode runs the hook `cocci-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'compile-command)

  (use-local-map cocci-mode-map)
  (set-syntax-table cocci-mode-syntax-table)
  (setq mode-name                "cocci"
        major-mode               'cocci-mode
        local-abbrev-table       cocci-mode-abbrev-table
	font-lock-defaults     '(cocci-font-lock-keywords)
	comment-start          "//"
	comment-end            ""
   )
  (easy-menu-add cocci-menu)

  (run-hooks 'cocci-mode-hook)
)


;; Menu

(easy-menu-define cocci-menu cocci-mode-map "Cocci menu"
  '("Cocci"
    ["Switch to corresponding C file" switch-between-cocci-c t]
    ["Replace modifiers" cocci-replace-modifiers t]
    ))



; put cursor before a parse error coccinelle message and it will
; open the corresponding file and go to corresponding line.
(fset 'cocci-goto-next-error
   [?\C-s ?F ?i ?l ?e right right ?\C-  ?\C-s ?" left ?\M-w ?\C-x ?\C-f S-insert return ?\C-\M-l C-right right C-S-right C-insert ?\C-\M-l ?\M-g S-insert return])
;"

;; Provide
(provide 'cocci-mode)

;;; cocci.el ends here
