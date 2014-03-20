;;; outshine-lang.el --- allow outshine mode to fold language elements
;; 
;;
;;; Commentary

;; This code extends Thorsten Jolitz's excellent outshine mode ( http://orgmode.org/worg/org-tutorials/org-outside-org.html
;; ) to allow folding of language elements: classes, functions, and other language constructs.  Language elements are
;; matched by user-specified regexps (see customization `outshine-lang-headline-regexps').  The outline levels of all
;; language element headlines are made to be greater than the levels of any standard outshine headlines, so that language
;; elements are always subordinate to outshine headlines.  The outline level of a language headline is taken to be a
;; large fixed constant (specified by customization `outshine-lang-max-standard-level') plus the amount of whitespace
;; preceding the language headline.  Since most code is pretty-printed in a hierarchical way (and Python is required to be),
;; this heuristic usually works well enough.

;;;; Implementation notes

;; This code works by redefining, via advice, `outline-regexp' and `outline-level' in outshine buffers.
;; The original values of these buffer-local variables (computed by outshine) are saved.
;; Then, `outline-regexp' is defined to match either standard or language headlines, and to capture the whitespace before
;; language headlines in a subexpression.  `outline-level' is defined to call the original `outline-level' implementation
;; from outshine for standard headlines, and for language headlines to use indentation level to determine outline level.
;; For promotion/demotion commands, `outline-regexp' and `outline-level' are restored to outshine's original values;
;; these commands then act only on standard headlines.

(require 'outshine)
(require 'rx)

;;; Customizations

(defgroup outshine-lang nil
  "Extension allowing outshine mode to fold language elements"
  :prefix "outshine-lang-"
  :group 'outshine)

(defcustom outshine-lang-max-standard-level 10
  "Max level of standard (non-language-expression) outline"
  :group 'outshine-lang
  :type 'integer)

(defcustom outshine-lang-headline-regexps
  `(("python" . ,(rx-to-string '(:
				 (or "def" "class"
				     "with" "for" "if"
				     "##"  ; to let the user insert headlines manually
				     )
				 space (1+ not-newline) (any "\:,") (0+ space) eol)))
    ("emacs-lisp" . ,(rx-to-string '(: "(def" (or "un" "var" "const" "group" "custom" "advice")))))
  "Alist of language-specific outline headlines for outshine mode.

The name of the language (key-string) should be the associated
major-mode name without the '-mode' suffix, eg python for python-mode'.
The value should be a regexp that matches language elements which should
be headlines.  The regexp should NOT match the whitespace before the language
element.
"
  :group 'outshine-lang
  :type '(alist :key-type string
		:value-type regexp))

;;; Variables for saving outline-regexp and outline-level computed by outshine

(defvar outshine-lang-orig-outline-regexp nil
  "Original value of `outline-regexp', before adding language headlines")
(make-variable-buffer-local 'outshine-lang-orig-outline-regexp)

(defvar outshine-lang-orig-outline-level nil
  "Original value of `outline-level', before adding language headlines")
(make-variable-buffer-local 'outshine-lang-orig-outline-level)

;;; Utility functions

(defun outshine-lang-get-language-name ()
  "Return language name for major-mode of current buffer.
Adapted from `navi-get-language-name'."
  (car
   (split-string
    (symbol-name major-mode)
    "-mode" 'OMIT-NULLS)))

(defun outshine-lang-get-headline-regexp ()
  "Return the language-element headline regexp for the current buffer's language,
or nil if headline regexps are not defined for that language."
  (cdr (assoc (outshine-lang-get-language-name) outshine-lang-headline-regexps)))

;;; Advice that modifies outshine behavior

(defadvice outline-static-level-p (around outshine-lang-set-lang-headline-level (level) activate)
  "Prevent promotion/demotion commands from touching language headlines"
  (setq ad-return-value
	(if (outshine-lang-get-headline-regexp)
	    (>= level outshine-lang-max-standard-level)
	  ad-do-it)))

(defadvice outline-change-level (around outshine-lang-change-level-revert-to-orig-outline-defs (delta) activate)
  "Revert to original expression"
  (let ((outline-regexp outline-regexp)
	(outline-level outline-level))
    (when (outshine-lang-get-headline-regexp)
      (setq outline-regexp outshine-lang-orig-outline-regexp
	    outline-level outshine-lang-orig-outline-level))
    ad-do-it))

(defun outshine-lang-activate ()
  "Turn language elems (function defs, class defs etc) into cycleable outshine-org headlines"
  (let ((lang-elem-headline-re (outshine-lang-get-headline-regexp)))
    (when lang-elem-headline-re
      (setq
       outshine-lang-orig-outline-regexp outline-regexp
       outshine-lang-orig-outline-level outline-level
       outline-regexp
       ;; Define a regexp which matches either language headlines or standard headlines.
       ;; If a language headline is matched, (match-string 1) gives matched headline and
       ;; (match-string 2) gives the whitespace preceding it; we take the length of the whitespace
       ;; to be the outline level.
       (rx-to-string
	`(or
	  (group
	   (group (0+ space))
	   (regexp ,lang-elem-headline-re))
	  (group (regexp ,outshine-lang-orig-outline-regexp))))
       outline-level
       (lambda ()
	 (if (match-string 1)
	     (+ outshine-lang-max-standard-level (length (match-string 2)))
	   (funcall outshine-lang-orig-outline-level)))))))

(defadvice outshine-hook-function (after outshine-lang-do-activate nil activate)
  (outshine-lang-activate))

(provide 'outshine-lang)
