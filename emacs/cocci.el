;;; cocci.el --- a major mode for editing semantic patches

;; Copyright (C) 2006 The Cocci Gang

;; Emacs Lisp Archive Entry
;; Author: Padioleau Yoann <padator@wanadoo.fr>
;; Version: 0.1
;; Keywords: coccinelle patch refactoring 
;; URL: http://www.emn.fr/x-info/coccinelle/

;; Please imagine a long and boring gnu-style copyright notice 
;; appearing just here.

;;; Usage
;; Add the following lines to your ~/.emacs or equivalent:
;;  (setq auto-mode-alist 
;;          (cons '("\\.cocci$" . cocci-mode) auto-mode-alist))
;;  (autoload 'cocci-mode "cocci" 
;;          "Major mode for editing cocci code." t)
;;
;; You can also use cocci-mode to edit the files containing the isomorphisms
;; with: 
;;  (setq auto-mode-alist 
;;           (cons '("\\.iso$" . cocci-mode) auto-mode-alist))

;;; Todo:
;; good compile command based on name of buffer.
;;  (defvar cocci-spatch-path)
;;  (defvar cocci-standard-isofile-path)
;; support to show ctl, ... enhance semantic-patch debugging experience
;; sgrep support ?

;;; Code:

(defun join-sep (sep xs) 
  (mapconcat 'identity xs sep))


;; new (color) faces

(make-face 'cocci-number-face)
(make-face 'cocci-punctuation-face)
(set-face-foreground 'cocci-number-face      "yellow3")
(set-face-foreground 'cocci-punctuation-face "cyan")

(make-face 'cocci-problem-face)
(make-face 'cocci-special-face)
(make-face 'cocci-minus-face)
(make-face 'cocci-plus-face)
(set-face-background 'cocci-problem-face      "deep pink")
(set-face-foreground 'cocci-special-face     "red")
(set-face-foreground 'cocci-minus-face     "SeaGreen3")
(set-face-foreground 'cocci-plus-face      "salmon")

;; can look in lexer_cocci.mll for new identifiers

(setq cocci-c-keywords-list 
      (list "if" "else" "while" "do" "for" "return" 
            "sizeof"
            "struct" "union"
            "static" "const" "volatile"
            "break" "continue"
            ))

(setq cocci-declaration-keywords-list 
      (list "identifier" "type" "parameter" "constant" "expression" "statement"
            "function" "local" "list" 
            "fresh" 
            ;"error" "words"

            "char" "short" "int" "float" "double" "long" 
            "void"
            "signed" "unsigned" 
            ))
      
(setq cocci-font-lock-keywords 
 `(
   ; blink possible errors, when - or + is not in first column
   ("^[ \\t]+[-+]" . 'cocci-problem-face)

   ; modifiers
   ;("^\\??\\+.*?//" . 'cocci-plus-face)
   ("^\\??\\+.*" . 'cocci-plus-face)
   ("^\\??-.*" . 'cocci-minus-face)
   ; ! \\+

   ; --- +++
   ; #cpp

   ; comments
   ("//.*" . 'font-lock-comment-face)

   ("@@" . 'cocci-special-face)

   ; just for pad
   ("\\b[A-Z][0-9]?\\b" . font-lock-variable-name-face)

   ; special cocci operators
   ("\\.\\.\\." . 'font-lock-keyword-face)
   ("^[()|]" . 'font-lock-keyword-face)
   ("\\bwhen[ \t]+!=" . 'font-lock-keyword-face)
   ("\\bWHEN[ \t]+!=" . 'font-lock-keyword-face)
   
   ; used in iso files
   ("<=>" . 'font-lock-keyword-face)
   ("=>" . 'font-lock-keyword-face)
   ("Expression" . 'cocci-special-face)
   ("Statement" . 'cocci-special-face)
   ("Declaration" . 'cocci-special-face)


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
  ;(define-key cocci-mode-map "\C-c" 'compile)
  )


(defvar cocci-mode-syntax-table nil 
  "Syntax table used while in cocci mode.")
(unless cocci-mode-syntax-table
  (setq cocci-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\_ "w"  cocci-mode-syntax-table) ; _ is part of a word.
  )




;; Used internally while developping coccinelle.
;; Allow to switch between the corresponding SP and C file.
;; todo: handle the _verxxx naming convention.
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

(define-key cocci-mode-map [(meta control *)] 'switch-between-cocci-c)
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

  (use-local-map cocci-mode-map)
  (set-syntax-table cocci-mode-syntax-table)
  (setq mode-name                "cocci"
        major-mode               'cocci-mode
        local-abbrev-table       cocci-mode-abbrev-table
	font-lock-defaults     '(cocci-font-lock-keywords)
	comment-start          "//"
	comment-end            ""
   )
  (run-hooks 'cocci-mode-hook)
)


(provide 'cocci-mode)

;;; cocci.el ends here
