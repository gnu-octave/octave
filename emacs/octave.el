;; octave-mode.el --- Octave mode for GNU Emacs

;;; Copyright (c) 1986, 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: John Eaton <jwe@bevo.che.wisc.edu>
;; Author: Kurt Hornik <hornik@ci.tuwien.ac.at>
;; Maintainer: bug-octave@bevo.che.wisc.edu
;; Version 0.6 (Nov 27 1995)
;; Keywords: languages

;; This file is not yet a part of GNU Emacs.  It is part of Octave.

;; Octave is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Octave is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Octave mode is based on Fortran mode written by Michael D. Prange
;; <prange@erl.mit.edu>.

;;; Bugs to bug-octave@bevo.che.wisc.edu

(defconst octave-mode-version "version 0.6")

;;; Code:

(defvar octave-stmt-indent 2
  "*Extra indentation applied to statements in block structures.")

(defvar octave-auto-newline nil
  "*Non nil means auto-insert newline and indent after semicolons.")

;; These next two should maybe be described someone makes it work again.

(defvar octave-comment-indent-style 'relative
  "*nil forces comment lines not to be touched,
'relative indents to current Octave indentation.  Default is 'relative")

(defvar octave-comment-column 32
  "Column to start in-line comments.")

(defvar octave-comment-start "#"
  "*Delimiter inserted to start new comment.")

(defvar octave-comment-line-start "#"
  "*Delimiter inserted to start comment on a new line.")

(defvar octave-comment-indent-char ?\ 
  "*Single-character inserted for Octave comment indentation.
Normally a space.")

(defvar octave-blink-matching-blocks t
  "*Non-nil causes \\[octave-indent-line] on end statements to blink the beginning of the block.")

(defvar octave-continuation-indent 4
  "*Extra indentation applied to Octave continuation lines.")

(defvar octave-continuation-string "\\"
  "Character string used for Octave continuation lines.
Normally \\.")

(defvar octave-comment-region "#$$$ "
  "*String inserted by \\[octave-comment-region]\
 at start of each line in region.")

(defvar octave-mode-abbrev-table nil)

(defvar octave-startup-message t
  "*Non-nil displays a startup message when Octave mode is first called.")

(defvar octave-mode-syntax-table nil
  "Syntax table in use in Octave mode buffers.")

(defvar octave-mode-map ()
  "Keymap used in Octave mode.")
(if octave-mode-map
    ()
  (setq octave-mode-map (make-sparse-keymap))
  (define-key octave-mode-map "`" 'octave-abbrev-start)
  (define-key octave-mode-map ";" 'octave-electric-semi)
  (define-key octave-mode-map "\C-c;" 'octave-comment-region)
  (define-key octave-mode-map "\C-c:" 'octave-un-comment-region)
  (define-key octave-mode-map "\e\C-a" 'octave-beginning-of-subprogram)
  (define-key octave-mode-map "\e\C-e" 'octave-end-of-subprogram)
  (define-key octave-mode-map "\e;" 'octave-indent-comment)
  (define-key octave-mode-map "\e\C-h" 'octave-mark-subprogram)
  (define-key octave-mode-map "\e\n" 'octave-split-line)
  (define-key octave-mode-map "\n" 'octave-indent-new-line)
  (define-key octave-mode-map "\e\C-q" 'octave-indent-subprogram)
  (define-key octave-mode-map "\C-c\C-p" 'octave-previous-statement)
  (define-key octave-mode-map "\C-c\C-n" 'octave-next-statement)
  (define-key octave-mode-map "\t" 'octave-indent-line))

;; menus

(require 'easymenu)

(easy-menu-define octave-mode-menu octave-mode-map
    "Menu used in Octave mode."
  (list "Octave"
	["Mark Subprogram"		octave-mark-subprogram t]
	["Indent Subprogram"		octave-indent-subprogram t]
	["Beginning of Subprogram"	octave-beginning-of-subprogram t]
	["End of Subprogram"		octave-end-of-subprogram t]
	"-"
	["Previous Statement"		octave-previous-statement t]
	["Next Statement"		octave-next-statement t]
	"-"
	["Comment Region"		octave-comment-region t]
	["Uncomment Region"		octave-un-comment-region t]
	"-"
	["Split Line at Point"		octave-split-line t]
	["Newline and Indent"		octave-indent-new-line t]
	["Indent Line"			octave-indent-line t]
	"-"
	["Describe Octave mode"		describe-mode t]))

(easy-menu-add octave-mode-menu octave-mode-map)

;; Some of these definitions are probably way too simple.

(defvar octave-continuation-regexp
  ".*\\(\\\\\\|\\.\\.\\.\\)[ \t]*\\([#%].*\\)?$")

(defvar octave-if-stmt-regexp "\\bif\\b")
(defvar octave-endif-stmt-regexp "\\bendif\\b")

(defvar octave-func-stmt-regexp "\\bfunction\\b")
(defvar octave-endfunc-stmt-regexp "\\bendfunction\\b")

(defvar octave-for-stmt-regexp "\\bfor\\b")
(defvar octave-endfor-stmt-regexp "\\bendfor\\b")

(defvar octave-try-stmt-regexp "\\btry\\b")
(defvar octave-endtry-stmt-regexp "\\bend_try_catch\\b")

(defvar octave-unwind-stmt-regexp "\\bunwind_protect\\b")
(defvar octave-endunwind-stmt-regexp "\\bend_unwind_protect\\b")

(defvar octave-while-stmt-regexp "\\bwhile\\b")
(defvar octave-endwhile-stmt-regexp "\\bendwhile\\b")

(defvar octave-end-block-kw
  "\\bend\\(for\\|function\\|if\\|_try_catch\\|_unwind_protect\\|while\\)?\\b")

(defvar octave-begin-block-kw
  "\\b\\(for\\|function\\|if\\|try\\|unwind_protect\\|while\\)\\b")

(defconst bug-octave-mode "bug-octave@bevo.che.wisc.edu"
  "Address of mailing list for Octave mode bugs.")

(defconst octave-comment-start-skip "[#%][ \t]*")

(defconst octave-comment-line-start-skip "^[ \t]*[#%]")

(if octave-mode-syntax-table
    ()
  (setq octave-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\r " " octave-mode-syntax-table)
  (modify-syntax-entry ?+ "." octave-mode-syntax-table)
  (modify-syntax-entry ?- "." octave-mode-syntax-table)
  (modify-syntax-entry ?= "." octave-mode-syntax-table)
  (modify-syntax-entry ?* "." octave-mode-syntax-table)
  (modify-syntax-entry ?/ "." octave-mode-syntax-table)
  (modify-syntax-entry ?> "." octave-mode-syntax-table)
  (modify-syntax-entry ?< "." octave-mode-syntax-table)
  (modify-syntax-entry ?& "." octave-mode-syntax-table)
  (modify-syntax-entry ?| "." octave-mode-syntax-table)
  (modify-syntax-entry ?! "." octave-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" octave-mode-syntax-table)
  (modify-syntax-entry ?\' "." octave-mode-syntax-table)
  (modify-syntax-entry ?\` "w" octave-mode-syntax-table)  ; for abbrevs
  (modify-syntax-entry ?\" "\"" octave-mode-syntax-table)
  (modify-syntax-entry ?. "w" octave-mode-syntax-table)
  (modify-syntax-entry ?_ "w" octave-mode-syntax-table)
  (modify-syntax-entry ?\% "<" octave-mode-syntax-table)
  (modify-syntax-entry ?\# "<" octave-mode-syntax-table)
  (modify-syntax-entry ?\n ">" octave-mode-syntax-table))

;;;###autoload
(defun octave-mode ()
  "Major mode for editing Octave code.

This mode makes it easier to write Octave code by helping with
indentation, doing some of the typing for you (with abbrevs-mode) and
by showing keywords, comments, strings, etc. in different faces (with
font-lock mode on terminals that support it).

Octave itself is a high-level language, primarily intended for
numerical computations.  It provides a convenient command line
interface for solving linear and nonlinear problems numerically.
Function definitions can also be stored in files, and it can be used
in a batch mode (which is why you need this mode!).

The latest released version of Octave is always available via
anonymous ftp from bevo.che.wisc.edu in the directory /pub/octave.
Complete source and binaries for several popular systems are
available.

\\[octave-indent-line] indents the current Octave line correctly. 
For this to work well, you should use the specific forms of end
statements (endif, endfor, endwhile, etc., and not just `end').

Type `? or `\\[help-command] to display a list of built-in abbrevs for
Octave keywords.

Keybindings
===========

\\{octave-mode-map}

Variables you can use to customize Octave mode
==============================================

octave-stmt-indent
  Extra indentation applied to statements in block structures.
  Default value is 2.

octave-auto-newline
  Non nil means auto-insert newline and indent after semicolons are
  typed.  The default value is nil.

octave-comment-start
  Delimiter inserted to start new comment.  Default value is \"#\".

octave-comment-line-start
  Delimiter inserted to start comment on a new line.  Default value
  is \"#\". 

octave-comment-indent-char
  Single-character inserted for Octave comment indentation.  Default
  value is a space.

octave-blink-matching-blocks
  Non-nil causes \\[octave-indent-line] on end statements to blink the
  beginning of the block.  Default value is t.

octave-continuation-indent
  Extra indentation applied to Octave continuation lines.  Default
  value is 4

octave-continuation-string
  String used for Octave continuation lines.  Normally \"\\\".

octave-comment-region
  String inserted by \\[octave-comment-region]\ at start of each line
  in region.  Default value is \"#$$$ \".

octave-startup-message
  Non-nil displays a startup message when Octave mode is first called.

Turning on Octave  mode calls the value of the variable `octave-mode-hook'
with no args, if that value is non-nil.

To begin using this mode for all .m files that you edit, add the
following lines to your .emacs file:

  (autoload 'octave-mode \"octave\" nil t)
  (setq auto-mode-alist (cons '(\"\\\\.m$\" . octave-mode) auto-mode-alist))

To turn on the abbrevs, auto-fill and font-lock features
automatically, also add the following lines to your .emacs file:

  (setq octave-mode-hook
        (list 'turn-on-auto-fill
              (lambda () ((abbrev-mode 1)
			  (if (eq window-system 'x)
			      (font-lock-mode))))))

See the Emacs manual for more information about how to customize font
lock mode."
  (interactive)

  (kill-all-local-variables)

  (if octave-startup-message
      (message
       "Octave mode %s.  Bugs to %s" octave-mode-version bug-octave-mode))
  (setq octave-startup-message nil)

  (setq local-abbrev-table octave-mode-abbrev-table)

  (set-syntax-table octave-mode-syntax-table)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(octave-font-lock-keywords nil nil))

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'octave-indent-line)

  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'octave-comment-hook)

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip octave-comment-start-skip)

  (use-local-map octave-mode-map)

  (setq mode-name "Octave")

  (setq major-mode 'octave-mode)

  (run-hooks 'octave-mode-hook))

;; Functions for indenting comments.

(defun octave-comment-hook ()
  (if (not (looking-at "\\s<"))
      (octave-calculate-indent)
    (skip-chars-backward " \t")
    (max (+ 1 (current-column)) octave-comment-column)))

(defun octave-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  (cond ((save-excursion
	   (beginning-of-line)
	   (looking-at octave-comment-line-start-skip))
	 (let ((icol 0) bol eol)
	   (save-excursion
	     (while (and (= 0 (forward-line -1))
			 (looking-at "^[ \t]*$")))
	     (progn
	       (setq eol (save-excursion (end-of-line) (point)))
	       (while (and (setq icol (re-search-forward "[#%]" eol t))
			   (octave-is-in-string-p (point))))))
	   (if icol
	       (progn
		 (save-excursion
		   (goto-char icol)
		   (setq icol (- (current-column) 1)))
		 (delete-horizontal-space)
		 (indent-to icol))
	     (delete-horizontal-space)
	     (indent-to (octave-calculate-indent))))
	 (if (looking-at "[#%][ \t]*$")
	     (end-of-line)))
	;; catches any inline comment and leaves point after
	;; octave-comment-start-skip
	((octave-find-comment-start-skip)
	 (if octave-comment-start-skip
	     (progn (goto-char (match-beginning 0))
		    (if (not (= (current-column) (octave-comment-hook)))
			(progn (delete-horizontal-space)
			       (indent-to (octave-comment-hook)))))
	   (end-of-line)))        ; otherwise goto end of line or sth else?
	;; No existing comment.  Insert separate-line comment, making
	;; a new line if necessary. 
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert octave-comment-line-start)
	 (insert-char octave-comment-indent-char
		      (- (octave-calculate-indent) (current-column))))))

(defun octave-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts octave-comment-region at the beginning of every line in the region.
BEG-REGION and END-REGION are args which specify the region boundaries.
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert octave-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert octave-comment-region)))
      (let ((com (regexp-quote octave-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)

	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun octave-un-comment-region (beg end)
  "Uncomments every line in the region."
  (interactive "*r")
  (octave-comment-region beg end 1))

(defun octave-split-line ()
  "Break line at point and insert continuation marker."
  (interactive)
  (delete-horizontal-space)
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at octave-comment-line-start-skip))
    (insert "\n" octave-comment-line-start " "))
   ((octave-is-in-string-p (point))
    (insert "\\\n "))
   (t
    (progn
      (insert octave-continuation-string)
      (insert "\n")
      (octave-indent-line)))))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun octave-beginning-of-subprogram ()
  "Moves point to the beginning of the current Octave subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward octave-func-stmt-regexp nil 'move)))

(defun octave-end-of-subprogram ()
  "Moves point to the end of the current Octave subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward octave-endfunc-stmt-regexp nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun octave-mark-subprogram ()
  "Put mark at end of Octave subprogram, point at beginning.
The marks are pushed."
  (interactive)
  (octave-end-of-subprogram)
  (push-mark (point))
  (octave-beginning-of-subprogram))

(defun octave-is-continuation-line ()
  "Returns t if the current line is a continuation line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\S<*\\(\\\\\\|\\.\\.\\.\\)[ \t]*\\(\\s<.*\\)?$")))

(defun octave-point (position)
  "Returns the value of point at certain positions." 
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (error "unknown buffer position requested: %s" position)))
    (point)))

(defun octave-previous-line ()
  "Moves point to beginning of the previous Octave code line (i.e.,
skips past all empty and comment lines."
  (interactive)
  (beginning-of-line)
  (while (progn
	   (forward-line -1)
	   (or (looking-at octave-comment-line-start-skip)
	       (looking-at "[ \t]*$")))))

(defun octave-previous-statement ()
  "Moves point to beginning of the previous Octave statement.
Returns `first-statement' if that statement is the first
non-comment Octave statement in the file, and nil otherwise."
  (interactive)
  (let ((eol (octave-point 'eol))
	not-first-statement)
    (beginning-of-line)
    (while (and (setq not-first-statement (= (forward-line -1) 0))
                (or (looking-at octave-comment-line-start-skip)
                    (looking-at "[ \t]*$")
		    (< (car (save-excursion
			      (parse-partial-sexp (octave-point 'bol)
						  eol)))
		       0)
                    (save-excursion
                      (forward-line -1)
                      (looking-at octave-continuation-regexp))
                    (looking-at
                     (concat "[ \t]*"  octave-comment-start-skip)))))
  (if (not not-first-statement)
      'first-statement)))

(defun octave-next-statement ()
  "Moves point to beginning of the next Octave statement.
Returns `last-statement' if that statement is the last
non-comment Octave statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
		      (and (= (forward-line 1) 0)
			   (not (eobp))))
 		(or (looking-at octave-comment-line-start-skip)
		    (save-excursion
		      (forward-line -1)
		      (looking-at octave-continuation-regexp))
 		    (looking-at "[ \t]*$")
 		    (looking-at
		     (concat "[ \t]*" octave-comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

;; Functions for marking if-endif, for-endfor, while-endwhile,
;; function-endfunction, unwind_protect-end_unwind_protect, and
;; try-end_try_catch blocks.

(defun octave-blink-matching-if ()
  ;; From a Octave endif statement, blink the matching if statement.
  (interactive)
  (octave-blink-matching-block
   octave-if-stmt-regexp octave-endif-stmt-regexp))

(defun octave-mark-if ()
  "Put mark at end of Octave if-endif construct, point at beginning.
The marks are pushed."
  (interactive)
  (octave-mark-block octave-if-stmt-regexp octave-endif-stmt-regexp))

(defun octave-blink-matching-function ()
  ;; From a Octave endfunction statement, blink the matching function
  ;; statement.
  (interactive)
  (octave-blink-matching-block
   octave-func-stmt-regexp octave-endfunc-stmt-regexp))

(defun octave-mark-function ()
  "Put mark at end of Octave function-endfunction construct, point at beg.
The marks are pushed."
  (interactive)
  (octave-mark-block octave-func-stmt-regexp octave-endfunc-stmt-regexp))

(defun octave-blink-matching-for ()
  ;; From a Octave endfor statement, blink the matching for statement.
  (interactive)
  (octave-blink-matching-block
   octave-for-stmt-regexp octave-endfor-stmt-regexp))

(defun octave-mark-for ()
  "Put mark at end of Octave for-endfor construct, point at beginning.
The marks are pushed."
  (interactive)
  (octave-mark-block octave-for-stmt-regexp octave-endfor-stmt-regexp))

(defun octave-blink-matching-try ()
  ;; From a Octave end_try_catch statement, blink the matching try statement.
  (interactive)
  (octave-blink-matching-block
   octave-try-stmt-regexp octave-endtry-stmt-regexp))

(defun octave-mark-try ()
  "Put mark at end of Octave try-endtry construct, point at beginning.
The marks are pushed."
  (interactive)
  (octave-mark-block octave-try-stmt-regexp octave-endtry-stmt-regexp))

(defun octave-blink-matching-unwind ()
  ;; From a Octave end_unwind_protect statement, blink the matching
  ;; unwind_protect statement.
  (interactive)
  (octave-blink-matching-block
   octave-unwind-stmt-regexp octave-endunwind-stmt-regexp))

(defun octave-mark-unwind ()
  "Put mark at end of Octave unwind construct, point at beginning.
The marks are pushed."
  (interactive)
  (octave-mark-block octave-unwind-stmt-regexp octave-endunwind-stmt-regexp))

(defun octave-blink-matching-while ()
  ;; From a Octave endwhile statement, blink the matching while statement.
  (interactive)
  (octave-blink-matching-block
   octave-while-stmt-regexp octave-endwhile-stmt-regexp))

(defun octave-mark-while ()
  "Put mark at end of Octave while-endwhile construct, point at beginning.
The marks are pushed."
  (interactive)
  (octave-mark-block octave-while-stmt-regexp octave-endwhile-stmt-regexp))

;; The functions that do all the work.

(defun octave-blink-matching-block (bb-re eb-re)
  (let ((top-of-window (window-start)) matching-block
	(endblock-point (point)) message)
    (if (save-excursion (beginning-of-line)
			(skip-chars-forward" \t")
			(looking-at eb-re))
	(progn
          (if (not (setq matching-block (octave-beginning-block bb-re eb-re)))
              (setq message "No beginning found for this block.")
            (if (< matching-block top-of-window)
                (save-excursion
                  (goto-char matching-block)
                  (beginning-of-line)
                  (setq message
                        (concat "Matches "
                                (buffer-substring
                                 (point) (progn (end-of-line) (point))))))))
	  (if message
	      (message "%s" message)
	    (goto-char matching-block)
	    (sit-for 1)
	    (goto-char endblock-point))))))

(defun octave-mark-block (bb-re eb-re)
  "Put mark at end of Octave FOR-ENDFOR construct, point at beginning.
The marks are pushed."
  (interactive)
  (let (endblock-point block-point)
    (if (setq endblock-point (octave-end-block bb-re eb-re))
        (if (not (setq block-point (octave-beginning-block bb-re eb-re)))
            (message "No beginning found for this block.")
          ;; Set mark, move point.
          (goto-char endblock-point)
	  (if (looking-at (concat eb-re "[ \t]*\\([%#].*\\)?"))
	      (forward-line 1)
	    (forward-char 5))
          (push-mark)
          (goto-char block-point)
	  (beginning-of-line)))))

(defun octave-end-block (bb-re eb-re)
  (if (save-excursion (beginning-of-line)
		      (skip-chars-forward " \t")
                      (looking-at eb-re))
      ;; Sitting on one.
      (match-beginning 0)
    ;; Search for one.
    (save-excursion
      (let ((count 1))
        (while (and (not (= count 0))
                    (not (eq (octave-next-statement) 'last-statement))
                    ;; Keep local to subprogram
                    (not (looking-at octave-endfunc-stmt-regexp)))

          (skip-chars-forward " \t")
          (cond ((looking-at eb-re)
                 (setq count (- count 1)))
                ((looking-at bb-re)
                 (setq count (+ count 1)))))
        (and (= count 0)
             ;; All pairs accounted for.
             (point))))))

(defun octave-beginning-block (bb-re eb-re)
  (if (save-excursion (beginning-of-line)
		      (skip-chars-forward " \t")
                      (looking-at bb-re))
      ;; Sitting on one.
      (match-beginning 0)
    ;; Search for one.
    (save-excursion
      (let ((count 1))
        (while (and (not (= count 0))
                    (not (eq (octave-previous-statement) 'first-statement))
                    ;; Keep local to subprogram
                    (not (looking-at octave-endfunc-stmt-regexp)))

          (skip-chars-forward " \t")
          (cond ((looking-at bb-re)
		 (setq count (- count 1)))
                ((looking-at eb-re)
		 (setq count (+ count 1)))))

	(and (= count 0)
	     ;; All pairs accounted for.
	     (point))))))

(defun octave-indent-line ()
  "Indents current Octave line based on its contents and on previous
lines."
  (interactive)
  (let ((cfi (octave-calculate-indent))
	(prev-indent (current-indentation))
	(prev-column (current-column)))
    (if (save-excursion
	  (beginning-of-line)
	  (and (not (looking-at octave-comment-line-start-skip))
	       (not (octave-find-comment-start-skip))))
	(progn
	  (beginning-of-line)
	  (delete-horizontal-space)
	  (indent-to cfi)
	  (if (> prev-column prev-indent)
	      (goto-char (+ (point) (- prev-column prev-indent)))))
      (octave-indent-comment))
    (if (and auto-fill-function
	     (> (save-excursion (end-of-line) (current-column))
		fill-column))
	(save-excursion
	  (end-of-line)
	  (octave-do-auto-fill)))
    (if octave-blink-matching-blocks
	(progn
	  (octave-blink-matching-if)
	  (octave-blink-matching-for)
	  (octave-blink-matching-try)
	  (octave-blink-matching-unwind)
	  (octave-blink-matching-while)
	  (octave-blink-matching-function)))))

(defun octave-indent-new-line ()
  "Reindent the current Octave line, insert a newline and indent the newline.
An abbrev before point is expanded if `abbrev-mode' is non-nil."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (or (looking-at "end")	;Reindent only where it is most
	    (looking-at "else")	;likely to be necessary
	    (looking-at octave-continuation-regexp))
	(octave-indent-line)))
  (end-of-line)
  (newline)
  (octave-indent-line))

(defun octave-indent-subprogram ()
  "Properly indents the Octave subprogram which contains point."
  (interactive)
  (save-excursion
    (octave-mark-subprogram)
    (message "Indenting function...")
    (indent-region (point) (mark) nil))
  (message "Indenting function...done."))

;; XELSE says whether to increment or decrement the count if we are
;; looking at an else-type statement.
;;
;; XEND says whether to decrement the count if we are looking at an
;; end-type statement.

(defun octave-calc-indent-this-line (xelse xend)
  (let ((icol 0)
	(have-if 0)
	(have-func 0)
	(have-uwp 0)
	(have-tc 0))
    (save-excursion
      (beginning-of-line)
      (while (< (point) (octave-point 'eol))
	(cond
	 ((and (looking-at "\\bif\\b")
	       (not (octave-is-in-string-p (point))))
	  (setq icol (+ icol octave-stmt-indent)
		have-if (+ have-if 1)))
	 ((and (looking-at "\\btry\\b")
	       (not (octave-is-in-string-p (point))))
	  (setq icol (+ icol octave-stmt-indent)
		have-tc (+ have-tc 1)))
	 ((and (looking-at "\\bunwind_protect\\b")
	       (not (octave-is-in-string-p (point))))
	  (setq icol (+ icol octave-stmt-indent)
		have-uwp (+ have-uwp 1)))
	 ((and (looking-at "\\bcatch\\b")
	       (not (octave-is-in-string-p (point))))
	  (if (eq have-tc 0)
	      (setq icol (+ icol (* xelse octave-stmt-indent)))))
	 ((and (looking-at "\\b\\(else\\|elseif\\)\\b")
	       (not (octave-is-in-string-p (point))))
	  (if (eq have-if 0)
	      (setq icol (+ icol (* xelse octave-stmt-indent)))))
	 ((and (looking-at "\\bunwind_protect_cleanup\\b")
	       (not (octave-is-in-string-p (point))))
	  (if (eq have-uwp 0)
	      (setq icol (+ icol (* xelse octave-stmt-indent)))))
	 ((and (looking-at "\\bend")
	       (not (octave-is-in-string-p (point))))
	  (progn
	    (if (> xend 0)
		(setq icol (- icol (* xend octave-stmt-indent)))
	      (if (> icol 0)
		  (setq icol (- icol octave-stmt-indent))))
	    (cond
	     ((and (> have-if 0) (looking-at "\\bendif\\b"))
	      (setq have-if (- have-if 1)))
	     ((and (> have-func 0) (looking-at "\\bendfunction\\b"))
	      (setq have-func (- have-func 1)))
	     ((and (> have-tc 0) (looking-at "\\bend_try_catch\\b"))
	      (setq have-tc (- have-tc 1)))
	     ((and (> have-uwp 0) (looking-at "\\bend_unwind_protect\\b"))
	      (setq have-uwp (- have-uwp 1))))))
	 ((and (looking-at octave-begin-block-kw)
	        (not (octave-is-in-string-p (point))))
	  (setq icol (+ icol octave-stmt-indent))))
	(forward-char)))
    icol))

(defun octave-calculate-indent ()
  "Calculates the Octave indent column based on previous lines."
  (interactive)
  (let* ((prev-line-is-continuation-line
	  (save-excursion
	    (octave-previous-line)
	    (octave-is-continuation-line)))
	 (indent-col 0)
	 first-statement)
    (save-excursion
      (beginning-of-line)
      (if (condition-case nil
	      (progn
		(up-list -1)
		t)
	    (error nil))
	  (setq indent-col (+ 1 (current-column)))
	(setq first-statement (octave-previous-statement))
	(if (not first-statement)
	    (progn
	      (skip-chars-forward " \t")
	      (setq indent-col (+ (octave-current-line-indentation)
				  (octave-calc-indent-this-line 1 0)))
	      (if (and prev-line-is-continuation-line
		       (octave-is-continuation-line))
		  (setq indent-col (+ indent-col
				      octave-continuation-indent)))))))

    ;; This fixes things if the line we are on is and else- or
    ;; end-type statement.
    (if (save-excursion
          (beginning-of-line)
          (skip-chars-forward " \t")
          (looking-at "\\(end\\|else\\|catch\\|unwind_protect_cleanup\\)"))
        (setq indent-col (+ indent-col
                            (octave-calc-indent-this-line -1 1))))
    indent-col))

(defun octave-electric-semi ()
  (interactive)
  (if (and (not (octave-is-in-string-p (point))) octave-auto-newline)
      (progn
	(insert ";")
	(octave-indent-new-line))
    (insert ";")))

(defun octave-current-line-indentation ()
  "Indentation of current line.  For comment lines, returns
indentation of the first non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    (if (looking-at octave-comment-line-start-skip)
	(progn
	  (goto-char (match-end 0))
	  (skip-chars-forward (char-to-string octave-comment-indent-char)))
      (skip-chars-forward " \t"))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun octave-find-comment-start-skip ()
  "Move to past `octave-comment-start-skip' found on current line.
Return t if `octave-comment-start-skip' found, nil if not."
;;; In order to move point only if octave-comment-start-skip is found,
;;; this one uses a lot of save-excursions.  Note that re-search-forward
;;; moves point even if octave-comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (if (save-excursion
	(re-search-forward octave-comment-start-skip
			   (save-excursion (end-of-line) (point)) t))
      (let ((save-match-beginning (match-beginning 0))
	    (save-match-end (match-end 0)))
	(if (octave-is-in-string-p (match-beginning 0))
	    (save-excursion
	      (goto-char save-match-end)
	      (octave-find-comment-start-skip)) ; recurse to end of line
	  (goto-char save-match-beginning)
	  (re-search-forward octave-comment-start-skip
			     (save-excursion (end-of-line) (point)) t)
	  (goto-char (match-end 0))
	  t))
    nil))

;;;From: simon@gnu (Simon Marshall)
;;; Find the next % or # not in a string.
(defun octave-match-comment (limit)
  (let (found)
    (while (and (setq found (re-search-forward "[#%]" limit t))
                (octave-is-in-string-p (point))))
    (if (not found)
	nil
      ;; Cheaper than `looking-at' "[#%].*".
      (store-match-data
       (list (1- (point)) (progn (end-of-line) (min (point) limit))))
      t)))

;;;From: ralf@up3aud1.gwdg.de (Ralf Fassel)
;;; Test if TAB format continuation lines work.
(defun octave-is-in-string-p (where)
  "Return non-nil if POS (a buffer position) is inside a Octave string,
nil else."
  (save-excursion
    (goto-char where)
    (cond
     ((bolp) nil)			; bol is never inside a string
     ((save-excursion			; comment lines too
	(beginning-of-line)(looking-at octave-comment-line-start-skip)) nil)
     (t (let (;; ok, serious now. Init some local vars:
	      (parse-state '(0 nil nil nil nil nil 0))
	      (quoted-comment-start (if comment-start
					(regexp-quote comment-start)))
	      (not-done t)
	      parse-limit
	      end-of-line
	      )
	  ;; move to start of current statement
	  (octave-next-statement)
	  (octave-previous-statement)
	  ;; now parse up to WHERE
	  (while not-done
	    (if (or ;; skip to next line if:
		 ;; - comment line?
		 (looking-at octave-comment-line-start-skip)
		 ;; - at end of line?
		 (eolp))
		;; get around a bug in forward-line in versions <= 18.57
		(if (or (> (forward-line 1) 0) (eobp))
		    (setq not-done nil))
	      ;; else:
	      ;; if we are at beginning of code line, skip any
	      ;; whitespace, labels and tab continuation markers.
	      (if (bolp) (skip-chars-forward " \t"))
	      ;; if we are in column <= 5 now, check for continuation char
	      (cond ((= 5 (current-column)) (forward-char 1))
		    ((and (< (current-column) 5)
			  (equal octave-continuation-string
				 (char-to-string (following-char)))
			  (forward-char 1))))
	      ;; find out parse-limit from here
	      (setq end-of-line (save-excursion (end-of-line)(point)))
	      (setq parse-limit (min where end-of-line))
	      ;; now parse if still in limits
	      (if (< (point) where)
		  (setq parse-state (parse-partial-sexp
				     (point) parse-limit nil nil parse-state))
		(setq not-done nil))
	      ))
	  ;; result is
	  (nth 3 parse-state))))))

(defun octave-auto-fill-mode (arg)
  "Toggle octave-auto-fill mode.
With ARG, turn `octave-auto-fill' mode on iff ARG is positive.
In `octave-auto-fill' mode, inserting a space at a column beyond `fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		     (> (prefix-numeric-value arg) 0))
		   'octave-indent-line
		 nil))
    (force-mode-line-update)))

(defun octave-do-auto-fill ()
  (interactive)
  (let* ((opoint (point))
	 (bol (save-excursion (beginning-of-line) (point)))
	 (eol (save-excursion (end-of-line) (point)))
	 (bos (min eol (+ bol (octave-current-line-indentation))))
	 (quote
	  (save-excursion
	    (goto-char bol)
	    (if (looking-at octave-comment-line-start-skip)
		nil			; OK to break quotes on comment lines.
	      (move-to-column fill-column)
	      (cond ((octave-is-in-string-p (point))
		     (save-excursion (re-search-backward "[^']'[^']" bol t)
				     (1+ (point))))
		    (t nil)))))
	 ;;
	 ;; decide where to split the line. If a position for a quoted
	 ;; string was found above then use that, else break the line
	 ;; before the last delimiter.
	 ;; Delimiters are whitespace, commas, and operators.
	 ;; Will break before a pair of *'s.
	 ;;
	 (fill-point
	  (or quote
	      (save-excursion
		(move-to-column (1+ fill-column))
		(skip-chars-backward "^ \t\n,'+-/*=)")
		(if (<= (point) (1+ bos))
		    (progn
		      (move-to-column (1+ fill-column))
;;;what is this doing???
		      (if (not (re-search-forward "[\t\n,'+-/*)=]" eol t))
			  (goto-char bol))))
		(if (bolp)
		    (re-search-forward "[ \t]" opoint t)
		  (forward-char -1)
		  (if (looking-at "'")
		      (forward-char 1)
		    (skip-chars-backward " \t\*")))
		(1+ (point)))))
	 )
    ;; if we are in an in-line comment, don't break unless the
    ;; line of code is longer than it should be. Otherwise
    ;; break the line at the column computed above.
    ;;
    ;; Need to use octave-find-comment-start-skip to make sure that
    ;; quoted # or %'s don't prevent a break.
    (if (not (or (save-excursion
		   (if (and (re-search-backward
			     octave-comment-start-skip bol t)
			    (not (octave-is-in-string-p (point))))
		       (progn
			 (skip-chars-backward " \t")
			 (< (current-column) (1+ fill-column)))))
		 (save-excursion
		   (goto-char fill-point)
		   (bolp))))
	(if (> (save-excursion
		 (goto-char fill-point) (current-column))
	       (1+ fill-column))
	    (progn (goto-char fill-point)
		   (octave-break-line))
	  (save-excursion
	    (if (> (save-excursion
		     (goto-char fill-point)
		     (current-column))
		   (+ (octave-calculate-indent) octave-continuation-indent))
		(progn
		  (goto-char fill-point)
		  (octave-break-line))))))
    ))

(defun octave-break-line ()
  (interactive)
  (let ((opoint (point))
	(bol (save-excursion (beginning-of-line) (point)))
	(eol (save-excursion (end-of-line) (point)))
	(comment-string nil))
    (save-excursion
      (if (and octave-comment-start-skip
	       (save-excursion
		 (beginning-of-line)
		 (octave-find-comment-start-skip)))
	  (progn
	    (re-search-backward octave-comment-line-start-skip bol t)
	    (setq comment-string (buffer-substring (point) eol))
	    (delete-region (point) eol))))

    (if comment-string
	(save-excursion
	  (goto-char bol)
	  (end-of-line)
	  (delete-horizontal-space)
	  (insert octave-comment-start)))))

;; Abbrevs.

(if octave-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'octave-mode-abbrev-table ())
    (define-abbrev octave-mode-abbrev-table  "`a" "all_va_args" nil)
    (define-abbrev octave-mode-abbrev-table  "`b" "break" nil)
    (define-abbrev octave-mode-abbrev-table  "`ca" "catch" nil)
    (define-abbrev octave-mode-abbrev-table  "`c" "continue" nil)
    (define-abbrev octave-mode-abbrev-table  "`el" "else" nil)
    (define-abbrev octave-mode-abbrev-table  "`eli" "elseif" nil)
    (define-abbrev octave-mode-abbrev-table  "`et" "end_try_catch" nil)
    (define-abbrev octave-mode-abbrev-table  "`eu" "end_unwind_protect" nil)
    (define-abbrev octave-mode-abbrev-table  "`ef" "endfor" nil)
    (define-abbrev octave-mode-abbrev-table  "`efu" "endfunction" nil)
    (define-abbrev octave-mode-abbrev-table  "`ei" "endif" nil)
    (define-abbrev octave-mode-abbrev-table  "`ew" "endwhile" nil)
    (define-abbrev octave-mode-abbrev-table  "`f" "for" nil)
    (define-abbrev octave-mode-abbrev-table  "`fu" "function" nil)
    (define-abbrev octave-mode-abbrev-table  "`gl" "global" nil)
    (define-abbrev octave-mode-abbrev-table  "`gp" "gplot" nil)
    (define-abbrev octave-mode-abbrev-table  "`gs" "gsplot" nil)
    (define-abbrev octave-mode-abbrev-table  "`if" "if ()" nil)
    (define-abbrev octave-mode-abbrev-table  "`rp" "replot" nil)
    (define-abbrev octave-mode-abbrev-table  "`r" "return" nil)
    (define-abbrev octave-mode-abbrev-table  "`t" "try" nil)
    (define-abbrev octave-mode-abbrev-table  "`up" "unwind_protect" nil)
    (define-abbrev octave-mode-abbrev-table  "`upc" "unwind_protect_cleanup" nil)
    (define-abbrev octave-mode-abbrev-table  "`w" "while ()" nil)
    (setq abbrevs-changed ac)))

(defun octave-abbrev-start ()
  "Typing `\\[help-command] or `? lists all the Octave abbrevs. 
Any other key combination is executed normally."
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (eq (setq c (read-event)) ??) ;; insert char if not equal to `?'
	    (eq c help-char))
	(octave-abbrev-help)
      (setq unread-command-events (list c)))))

(defun octave-abbrev-help ()
  "List the currently defined abbrevs in Octave mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (octave-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun octave-prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (insert-abbrev-table-description 'octave-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

;; Font-lock stuff.

;; Regexps (for the Fortran mode that this was originally based on)
;; done by simon@gnu with help from Ulrik Dickow <dickow@nbi.dk> and
;; probably others Si's forgotten about (sorry).

(defvar octave-font-lock-keywords-1
  (let ((comment-chars "%#"))
    (list
     ;; Fontify comments.
     (cons (concat "[" comment-chars "].*$")
	   'font-lock-comment-face)
     ;; Function declarations.
     (list "\\<\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
           '(1 font-lock-keyword-face)
           '(2 font-lock-function-name-face nil t))))
  "For consideration as a value of `octave-font-lock-keywords'.
This does fairly subdued highlighting.")

(defvar octave-font-lock-keywords
  (append octave-font-lock-keywords-1
	  (let ((fkeywords
		 "\\(all_va_args\\|break\\|catch\\|continue\\|else\\|elseif\\|end_try_catch\\|end_unwind_protect\\|endfor\\|endfunction\\|endif\\|endwhile\\|end\\|for\\|function\\|global\\|gplot\\|gsplot\\|if\\|replot\\|return\\|try\\|unwind_protect\\|unwind_protect_cleanup\\|while\\)")
		(flogicals
		 "\\(&&\\|||\\|<=\\|>=\\|==\\|<\\|>\\|!=\\|!\\)"))
	    (list
	     ;; Fontify all builtin keywords.
	     (cons (concat "\\<\\(" fkeywords "\\)\\>")
		   'font-lock-keyword-face)
	     ;; Fontify all builtin operators.
	     (cons (concat "\\(" flogicals "\\)")
		   'font-lock-reference-face))))
  "For consideration as a value of `octave-font-lock-keywords'.
This does a lot more highlighting.")

(provide 'octave-mode)

;; Compile this file when saving it:

;;; Local Variables:
;;; after-save-hook: ((lambda () (byte-compile-file buffer-file-name)))
;;; End:
