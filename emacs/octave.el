;; octave.el --- Octave mode for GNU Emacs

;;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
;; Author: John Eaton <jwe@bevo.che.wisc.edu>
;; Maintainer: Kurt Hornik <Kurt.Hornik@ci.tuwien.ac.at>
;; Version: 0.8.7
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

;; This package provides a major mode for editing Octave code.  It knows
;; about Octave syntax and comments.  It also provides a major mode for
;; interacting with an inferior Octave process using comint, both
;; directly and by sending parts of a file with Octave code.

;;; Code:

(defconst octave-version "0.8.7")
(defconst octave-help-address
  "Kurt.Hornik@ci.tuwien.ac.at"
  "Address for Octave mode bug reports")


(defvar octave-mode-map nil
  "Keymap used in Octave mode.")
(if octave-mode-map
    ()
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'octave-abbrev-start)
    (define-key map ";" 'octave-electric-semi)
    (define-key map " " 'octave-electric-space)
    (define-key map "\n" 'octave-reindent-then-newline-and-indent)
    (define-key map "\t" 'indent-according-to-mode)
    (define-key map "\e;" 'octave-indent-for-comment)
    (define-key map "\e\n" 'octave-indent-new-comment-line)  
    (define-key map "\e\t" 'octave-complete-symbol)
    (define-key map "\M-\C-a"	'octave-beginning-of-defun)
    (define-key map "\M-\C-e" 'octave-end-of-defun)
    (define-key map "\M-\C-h" 'octave-mark-defun)
    (define-key map "\M-\C-q" 'octave-indent-defun)  
    (define-key map "\C-c;" 'octave-comment-region)
    (define-key map "\C-c:" 'octave-uncomment-region)  
    (define-key map "\C-c\C-b" 'octave-submit-bug-report)
    (define-key map "\C-c\C-p" 'octave-previous-code-line)
    (define-key map "\C-c\C-n" 'octave-next-code-line)
    (define-key map "\C-c\C-a" 'octave-beginning-of-line)
    (define-key map "\C-c\C-e" 'octave-end-of-line)  
    (define-key map "\C-c\M-\C-n" 'octave-forward-block)
    (define-key map "\C-c\M-\C-p" 'octave-backward-block)
    (define-key map "\C-c\M-\C-u" 'octave-backward-up-block)
    (define-key map "\C-c\M-\C-d" 'octave-down-block)
    (define-key map "\C-c\M-\C-h" 'octave-mark-block)
    (define-key map "\C-c]" 'octave-close-block)
    (define-key map "\C-cf" 'octave-insert-defun)
    (define-key map "\C-c\C-h" 'octave-help)
    (define-key map "\C-cil" 'octave-send-line)
    (define-key map "\C-cib" 'octave-send-block)
    (define-key map "\C-cif" 'octave-send-defun)
    (define-key map "\C-cir" 'octave-send-region)  
    (define-key map "\C-cis" 'octave-show-process-buffer)
    (define-key map "\C-cih" 'octave-hide-process-buffer)
    (define-key map "\C-cik" 'octave-kill-process)
    (setq octave-mode-map map)))

(defvar octave-mode-menu
  (list "Octave"
	(list "Lines"
	      ["Previous Code Line"	octave-previous-code-line t]
	      ["Next Code Line"		octave-next-code-line t]
	      ["Begin of Continuation"	octave-beginning-of-line t]
	      ["End of Continuation"	octave-end-of-line t]
	      ["Split Line at Point"	octave-indent-new-comment-line t])
	(list "Blocks"
	      ["Next Block"		octave-forward-block t]
	      ["Previous Block"		octave-backward-block t]
	      ["Down Block"		octave-down-block t]
	      ["Up Block"		octave-backward-up-block t]
	      ["Mark Block"		octave-mark-block t]
	      ["Close Block"		octave-close-block t])
	(list "Functions"
	      ["Begin of Function"	octave-beginning-of-defun t]
	      ["End of Function"	octave-end-of-defun t]
	      ["Mark Function"		octave-mark-defun t]
	      ["Indent Function"	octave-indent-defun t]
	      ["Insert Function"	octave-insert-defun t])
	"-"
	(list "Debug"
	      ["Send Current Line"	octave-send-line t]
	      ["Send Current Block"	octave-send-block t]
	      ["Send Current Function"	octave-send-defun t]
	      ["Send Region"		octave-send-region t]
	      ["Show Process Buffer"	octave-show-process-buffer t]
	      ["Hide Process Buffer"	octave-hide-process-buffer t]
	      ["Kill Process"		octave-kill-process t])
	"-"
	["Indent Line"			indent-according-to-mode t]
	["Complete Symbol"		octave-complete-symbol t]
	"-"
	["Toggle Abbrev Mode"		abbrev-mode t]
	["Toggle Auto-Fill Mode"	auto-fill-mode t]
	"-"
	["Submit Bug Report"		octave-submit-bug-report t]
	"-"
	["Describe Octave Mode"		octave-describe-major-mode t]
	["Lookup Octave Index"		octave-help t])
  "Menu for Octave mode.")

(defvar octave-mode-abbrev-table nil
  "Abbrev table in use in Octave mode buffers.")
(if octave-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'octave-mode-abbrev-table ())
    ;; By default, abbrevs are provided for all reserved words.
    (define-abbrev octave-mode-abbrev-table "`a" "all_va_args" nil)
    (define-abbrev octave-mode-abbrev-table "`b" "break" nil)
    (define-abbrev octave-mode-abbrev-table "`ca" "catch" nil)
    (define-abbrev octave-mode-abbrev-table "`c" "continue" nil)
    (define-abbrev octave-mode-abbrev-table "`el" "else" nil)
    (define-abbrev octave-mode-abbrev-table "`eli" "elseif" nil)
    (define-abbrev octave-mode-abbrev-table "`et" "end_try_catch" nil)
    (define-abbrev octave-mode-abbrev-table "`eu" "end_unwind_protect"
      nil)
    (define-abbrev octave-mode-abbrev-table "`ef" "endfor" nil)
    (define-abbrev octave-mode-abbrev-table "`efu" "endfunction" nil)
    (define-abbrev octave-mode-abbrev-table "`ei" "endif" nil)
    (define-abbrev octave-mode-abbrev-table "`ew" "endwhile" nil)
    (define-abbrev octave-mode-abbrev-table "`f" "for" nil)
    (define-abbrev octave-mode-abbrev-table "`fu" "function" nil)
    (define-abbrev octave-mode-abbrev-table "`gl" "global" nil)
    (define-abbrev octave-mode-abbrev-table "`gp" "gplot" nil)
    (define-abbrev octave-mode-abbrev-table "`gs" "gsplot" nil)
    (define-abbrev octave-mode-abbrev-table "`if" "if ()" nil)
    (define-abbrev octave-mode-abbrev-table "`rp" "replot" nil)
    (define-abbrev octave-mode-abbrev-table "`r" "return" nil)
    (define-abbrev octave-mode-abbrev-table "`t" "try" nil)
    (define-abbrev octave-mode-abbrev-table "`up" "unwind_protect" nil)
    (define-abbrev octave-mode-abbrev-table "`upc"
      "unwind_protect_cleanup" nil)
    (define-abbrev octave-mode-abbrev-table "`w" "while ()" nil)
    (setq abbrevs-changed ac)))

(defvar octave-comment-char ?#
  "The character to start a comment.")
(defvar octave-comment-column 32
  "*Column to indent in-line comments to.")
(defvar octave-comment-start
  (concat (make-string 1 octave-comment-char) " ")
  "String to insert to start a new in-line Octave comment.")
(defvar octave-block-comment-start
  (concat (make-string 2 octave-comment-char) " ")
  "String to insert to start a new Octave comment on an empty line.")
(defvar octave-comment-start-skip "\\s<+\\s-*"
  "Regexp to match the start of an Octave comment plus everything up to
its body.")
(defvar octave-fill-column 72
  "*Column beyond which automatic line-wrapping should happen.")

(defvar octave-mode-syntax-table nil
  "Syntax table in use in Octave mode buffers.")
(if octave-mode-syntax-table
    ()
  (setq octave-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\r " "  octave-mode-syntax-table)
  (modify-syntax-entry ?+ "."   octave-mode-syntax-table)
  (modify-syntax-entry ?- "."   octave-mode-syntax-table)
  (modify-syntax-entry ?= "."   octave-mode-syntax-table)
  (modify-syntax-entry ?* "."   octave-mode-syntax-table)
  (modify-syntax-entry ?/ "."   octave-mode-syntax-table)
  (modify-syntax-entry ?> "."   octave-mode-syntax-table)
  (modify-syntax-entry ?< "."   octave-mode-syntax-table)
  (modify-syntax-entry ?& "."   octave-mode-syntax-table)
  (modify-syntax-entry ?| "."   octave-mode-syntax-table)
  (modify-syntax-entry ?! "."   octave-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" octave-mode-syntax-table)
  (modify-syntax-entry ?\' "."  octave-mode-syntax-table)
  (modify-syntax-entry ?\` "w"  octave-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" octave-mode-syntax-table)
  (modify-syntax-entry ?. "w"   octave-mode-syntax-table)
  ;; Not sure if we should do this ...
  (modify-syntax-entry ?_ "w"   octave-mode-syntax-table)
  (modify-syntax-entry ?\% "."  octave-mode-syntax-table)
  (modify-syntax-entry ?\# "<"  octave-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  octave-mode-syntax-table))

(defvar octave-begin-keywords
  '("for" "function" "if" "try" "unwind_protect" "while"))
(defvar octave-else-keywords
  '("catch" "else" "elseif" "unwind_protect_cleanup"))
(defvar octave-end-keywords
  '("end" "endfor" "endfunction" "endif" "end_try_catch"
    "end_unwind_protect" "endwhile"))

(defvar octave-blink-matching-block t
  "*Non-nil means show matching begin of block when inserting a space,
newline or `;' after an else or end keyword.")
(defvar octave-block-offset 2
  "*Extra indentation applied to statements in block structures.")
(defvar octave-block-begin-regexp
  (concat "\\<\\("
	  (mapconcat 'identity octave-begin-keywords "\\|")
	  "\\)\\>"))
(defvar octave-block-else-regexp
  (concat "\\<\\("
	  (mapconcat 'identity octave-else-keywords "\\|")
	  "\\)\\>"))
(defvar octave-block-end-regexp
  (concat "\\<\\("
	  (mapconcat 'identity octave-end-keywords "\\|")
	  "\\)\\>"))
(defvar octave-block-begin-or-end-regexp
  (concat octave-block-begin-regexp "\\|" octave-block-end-regexp))
(defvar octave-block-else-or-end-regexp
  (concat octave-block-else-regexp "\\|" octave-block-end-regexp))
(defvar octave-block-match-alist
  '(("for" . ("end" "endfor"))
    ("function" . ("end" "endfunction"))
    ("if" . ("else" "elseif" "end" "endif"))
    ("try" . ("catch" "end" "end_try_catch"))
    ("unwind_protect" . ("unwind_protect_cleanup" "end"
			 "end_unwind_protect"))
    ("while" . ("end" "endwhile")))
  "Alist with Octave's begin keywords as keys and a list of the matchin
else or end keywords as associated values.")

(defvar octave-function-header-regexp
  (concat "^\\s-*\\<\\(function\\)\\>"
	  "\\([^=;\n]*=[ \t]*\\|[ \t]*\\)\\(\\w+\\)\\>")
  "Regexp to match a function header.  The string `function' and its
name are given by the first and third parenthetical grouping.")

(defvar octave-continuation-offset 4
  "*Extra indentation applied to Octave continuation lines.")  
(defvar octave-continuation-regexp
  "[^#%\n]*\\(\\\\\\|\\.\\.\\.\\)\\s-*\\(\\s<.*\\)?$")
(defvar octave-continuation-string "\\"
  "*Character string used for Octave continuation lines.  Normally \\.")

(defvar octave-reserved-words
  (append octave-begin-keywords octave-else-keywords octave-end-keywords
	  '("all_va_args" "break" "continue" "global" "gplot" "gsplot"
	    "replot" "return"))
  "Reserved words in Octave.")

(defvar octave-text-functions
  '("casesen" "cd" "chdir" "clear" "diary" "dir" "document" "echo"
    "edit_history" "format" "gset" "gshow" "help" "history" "hold"
    "load" "ls" "more" "run_history" "save" "set" "show" "type"
    "which" "who" "whos")
  "Octave text functions (these names are also reserved.")

(defvar octave-variables
  '("EDITOR" "EXEC_PATH" "F_DUPFD" "F_GETFD" "F_GETFL" "F_SETFD"
    "F_SETFL" "I" "IMAGEPATH" "INFO_FILE" "INFO_PROGRAM" "Inf" "J"
    "LOADPATH" "NaN" "OCTAVE_VERSION" "O_APPEND" "O_CREAT" "O_EXCL"
    "O_NONBLOCK" "O_RDONLY" "O_RDWR" "O_TRUNC" "O_WRONLY" "PAGER" "PS1"
    "PS2" "PS4" "PWD" "SEEK_CUR" "SEEK_END" "SEEK_SET" "__F_DUPFD__"
    "__F_GETFD__" "__F_GETFL__" "__F_SETFD__" "__F_SETFL__" "__I__"
    "__Inf__" "__J__" "__NaN__" "__OCTAVE_VERSION__" "__O_APPEND__"
    "__O_CREAT__" "__O_EXCL__" "__O_NONBLOCK__" "__O_RDONLY__"
    "__O_RDWR__" "__O_TRUNC__" "__O_WRONLY__" "__PWD__" "__SEEK_CUR__"
    "__SEEK_END__" "__SEEK_SET__" "__argv__" "__e__" "__eps__"
    "__error_text__" "__i__" "__inf__" "__j__" "__nan__" "__pi__"
    "__program_invocation_name__" "__program_name__" "__realmax__"
    "__realmin__" "__stderr__" "__stdin__" "__stdout__" "ans" "argv"
    "automatic_replot" "beep_on_error" "completion_append_char"
    "default_return_value" "default_save_format"
    "define_all_return_values" "do_fortran_indexing" "e"
    "echo_executing_commands" "empty_list_elements_ok" "eps"
    "error_text" "gnuplot_binary" "gnuplot_has_multiplot" "history_file"
    "history_size" "ignore_function_time_stamp" "implicit_str_to_num_ok"
    "inf" "nan" "nargin" "ok_to_lose_imaginary_part"
    "output_max_field_width" "output_precision"
    "page_output_immediately" "page_screen_output" "pi"
    "prefer_column_vectors" "prefer_zero_one_indexing"
    "print_answer_id_name" "print_empty_dimensions"
    "program_invocation_name" "program_name" "propagate_empty_matrices"
    "realmax" "realmin" "resize_on_range_error"
    "return_last_computed_value" "save_precision" "saving_history"
    "silent_functions" "split_long_rows" "stderr" "stdin" "stdout"
    "string_fill_char" "struct_levels_to_print"
    "suppress_verbose_help_message" "treat_neg_dim_as_zero"
    "warn_assign_as_truth_value" "warn_comma_in_global_decl"
    "warn_divide_by_zero" "warn_function_name_clash"
    "warn_missing_semicolon" "whitespace_in_literal_matrix")
  "Builtin variables in Octave.")

(defvar octave-completion-alist nil
  "Alist of Octave command and variable names that should be included in
completion.  Each element looks like (VAR . VAR), where the car and cdr
are the same symbol.")

(defvar octave-font-lock-keywords
  (list
   ;; Fontify all builtin keywords.
   (cons (concat "\\<\\("
		 (mapconcat 'identity octave-reserved-words "\\|")
		 (mapconcat 'identity octave-text-functions "\\|")
		 "\\)\\>")
	 'font-lock-keyword-face)
   ;; Fontify all builtin operators.
   (cons "\\(&\\||\\|<=\\|>=\\|==\\|<\\|>\\|!=\\|!\\)"
	 'font-lock-reference-face)
   ;; Fontify all builtin variables.
   (cons (concat "\\<\\("
		 (mapconcat 'identity octave-variables "\\|")
		 "\\)\\>")
	 'font-lock-variable-name-face)
   ;; Fontify all function declarations.
   (list octave-function-header-regexp
	 '(1 font-lock-keyword-face)
	 '(3 font-lock-function-name-face nil t)))
  "Additional expressions to highlight in Octave mode.")

(defvar octave-imenu-generic-expression
  (list
   ;; Functions
   (list nil octave-function-header-regexp 3))
  "Imenu expression for Octave mode.  See `imenu-generic-expression'.")

(defvar octave-help-files
  '("octave")
  "List of info files with documentation for Octave.
Default is '(\"octave\").")

(defvar octave-help-lookup-alist nil
  "Alist of Octave index entries for lookup")

(defvar octave-help-completion-alist nil
  "Alist of Octave index entries for completion.
The entries are of the form (VAR . VAR), where the VAR runs through all
different keys in octave-help-lookup-alist.")

(defvar octave-help-index-alternatives nil
  "List of matches for last octave-help command.")

(defvar octave-auto-newline nil
  "*Non-nil means automatically insert a newline and indent after a
semicolon is typed.")

(defvar octave-inhibit-startup-message nil
  "*If non-nil, the startup message will not be displayed.")

(defvar octave-mode-hook nil
  "*Hook to be run when Octave Mode is started.")


;;;###autoload
(defun octave-mode ()
  "Major mode for editing Octave code.

This mode makes it easier to write Octave code by helping with
indentation, doing some of the typing for you (with abbrevs-mode) and by
showing keywords, comments, strings, etc. in different faces (with
font-lock mode on terminals that support it).

Octave itself is a high-level language, primarily intended for numerical
computations.  It provides a convenient command line interface for
solving linear and nonlinear problems numerically.  Function definitions
can also be stored in files, and it can be used in a batch mode (which
is why you need this mode!).

The latest released version of Octave is always available via anonymous
ftp from bevo.che.wisc.edu in the directory /pub/octave.  Complete
source and binaries for several popular systems are available.

Type `? or `\\[help-command] to display a list of built-in abbrevs for \
Octave keywords.

Keybindings
===========

\\{octave-mode-map}

Variables you can use to customize Octave mode
==============================================

octave-auto-newline
  Non-nil means auto-insert a newline and indent after a semicolon.
  Default is nil.

octave-blink-matching-block
  Non-nil means show matching begin of block when inserting a space,
  newline or `;' after an else or end keyword.  Default is t.

octave-block-offset
  Extra indentation applied to statements in block structures.
  Default is 2.

octave-comment-column
  Column to indent right-margin comments to.  Default is 32.

octave-continuation-offset
  Extra indentation applied to Octave continuation lines.  Default is 4.

octave-continuation-string
  String used for Octave continuation lines.  Default is a backslash.

octave-fill-column
  Column beyond which automatic line-wrapping should happen.  Default is
  72.

octave-help-files
  List of info files with documentation for Octave.
  Default is '(\"octave\").

octave-inhibit-startup-message
  If t, no startup message is displayed when Octave mode is called.
  Default is nil.

octave-send-echo-input
  Non-nil means always display inferior-octave-buffer after sending a
  command.

octave-send-line-auto-forward
  Non-nil means always go to the next unsent line of Octave code after
  sending a line to the inferior process.

octave-send-echo-input
  Non-nil means echo input sent to the inferior Octave process.

Turning on Octave mode runs the hook `octave-mode-hook'.

To begin using this mode for all .m files that you edit, add the
following lines to your .emacs file:

  (autoload 'octave-mode \"octave\" nil t)
  (setq auto-mode-alist
        (cons '(\"\\\\.m$\" . octave-mode) auto-mode-alist))

To automatically turn on the abbrev, auto-fill and font-lock features,
add the following lines to your .emacs file as well:

  (add-hook 'octave-mode-hook
	    (lambda ()
	      (abbrev-mode 1)
	      (auto-fill-mode 1)
	      (if (eq window-system 'x)
		  (font-lock-mode 1))))

To submit a problem report, enter `\\[octave-submit-bug-report]' from \
an Octave mode buffer.
This automatically sets up a mail buffer with version information
already added.  You just need to add a description of the problem,
including a reproducible test case and send the message."
  (interactive)
  (kill-all-local-variables)

  (use-local-map octave-mode-map)  
  (setq major-mode 'octave-mode)
  (setq mode-name "Octave")
  (setq local-abbrev-table octave-mode-abbrev-table)  
  (set-syntax-table octave-mode-syntax-table)
  
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'octave-indent-line)

  (make-local-variable 'comment-start)  
  (setq comment-start octave-comment-start)
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column octave-comment-column)    
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip octave-comment-start-skip)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'octave-comment-indent)

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "\\s-*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'octave-fill-paragraph)
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp nil)
  (make-local-variable 'fill-column)
  (setq fill-column octave-fill-column)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(octave-font-lock-keywords nil nil))

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression octave-imenu-generic-expression)

  (octave-add-octave-menu)
  (octave-initialize-completions)
  (run-hooks 'octave-mode-hook)
  (if (not octave-inhibit-startup-message)
      (message "Octave mode %s.  Bugs to %s"
	       octave-version octave-help-address)))


;;; Miscellaneous useful functions
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

(defsubst octave-in-comment-p ()
  "Returns t if point is inside an Octave comment, nil otherwise."
  (interactive)
  (save-excursion
    (nth 4 (parse-partial-sexp (octave-point 'bol) (point)))))

(defsubst octave-in-string-p ()
  "Returns t if point is inside an Octave string, nil otherwise."
  (interactive)
  (save-excursion
    (nth 3 (parse-partial-sexp (octave-point 'bol) (point)))))

(defsubst octave-not-in-string-or-comment-p ()
  "Returns t iff point is not inside an Octave string or comment."
  (let ((pps (parse-partial-sexp (octave-point 'bol) (point))))
    (not (or (nth 3 pps) (nth 4 pps)))))

(defun octave-in-block-p ()
  "Returns t iff point is inside a block, which is taken to start at the
first letter of the begin keyword and to end after the end keyword." 
  (let ((pos (point)))
    (save-excursion
      (condition-case nil
	  (progn
	    (skip-syntax-forward "\\w")
	    (octave-up-block -1)
	    (octave-forward-block)
	    t)
	(error nil))
      (< pos (point)))))

(defun octave-in-defun-p ()
  "Returns t iff point is inside a function declaration, which is taken
to start at the `f' of `function' and to end after the end keyword."
  (let ((pos (point)))
    (save-excursion
      (or (and (looking-at "\\<function\\>")
	       (octave-not-in-string-or-comment-p))
	  (and (octave-beginning-of-defun)
	       (condition-case nil
		   (progn
		     (octave-forward-block)
		     t)
		 (error nil))
	       (< pos (point)))))))

;;; Abbrevs
(defun octave-abbrev-start ()
  "Typing `\\[help-command] or `? lists all abbrevs. 
Any other key combination is executed normally."
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (eq (setq c (read-event)) ??)
            (eq c help-char))
        (list-abbrevs)
      (setq unread-command-events (list c)))))


;;; Comments
(defun octave-comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.  See `comment-region'."
  (interactive "r\nP")
  (let ((comment-start (char-to-string octave-comment-char)))
    (comment-region beg end arg)))
  
(defun octave-uncomment-region (beg end &optional arg)
  "Uncomment each line in the region."
  (interactive "r\nP")
  (or arg (setq arg 1))
  (octave-comment-region beg end (- arg)))


;;; Indentation
(defun calculate-octave-indent ()
  "Return appropriate indentation for current line as Octave code.
Returns an integer (the column to indent to) unless the line is a
comment line with fixed goal golumn.  In that case, returns a list whose
car is the column to indent to, and whose cdr is the current indentation
level."
  (let ((is-continuation-line
	 (save-excursion
	   (if (zerop (octave-previous-code-line))
	       (looking-at octave-continuation-regexp))))
	(icol 0))
    (save-excursion
      (beginning-of-line)
      ;; If we can move backward out one level of parentheses, take 1
      ;; plus the indentation of that parenthesis.  Otherwise, go back
      ;; to the beginning of the previous code line, and compute the
      ;; offset this line gives.
      (if (condition-case nil
	      (progn
		(up-list -1)
		t)
	    (error nil))
	  (setq icol (+ 1 (current-column)))
	(if (zerop (octave-previous-code-line))
	    (progn
	      (octave-beginning-of-line)
	      (back-to-indentation)
	      (setq icol (current-column))
	      (let ((bot (point))
		    (eol (octave-point 'eol)))
		(while (< (point) eol)
		  (if (octave-not-in-string-or-comment-p)
		      (cond
		       ((looking-at octave-block-begin-regexp)
			(setq icol (+ icol octave-block-offset)))
		       ((looking-at octave-block-else-regexp)
			(if (= bot (point))
			    (setq icol (+ icol octave-block-offset))))
		       ((looking-at octave-block-end-regexp)
			(if (not (= bot (point)))
			    (setq icol (- icol octave-block-offset))))))
		  (forward-char)))
	      (if is-continuation-line
		  (setq icol (+ icol octave-continuation-offset)))))))
    (save-excursion
      (back-to-indentation)
      (cond
       ((and (or (looking-at octave-block-else-regexp)
		 (looking-at octave-block-end-regexp))
	     (octave-not-in-string-or-comment-p))
	(setq icol (- icol octave-block-offset)))
       ((looking-at "\\s<\\s<\\s<\\S<")
	(setq icol (list 0 icol)))
       ((looking-at "\\s<\\S<")
	(setq icol (list octave-comment-column icol)))))
    icol))

(defun octave-comment-indent ()
  (if (looking-at "\\s<\\s<\\s<")
      0
    (if (looking-at "\\s<\\s<")
	(calculate-octave-indent)
      (skip-syntax-backward "\\s-")
      (max (if (bolp) 0 (+ (current-column)))
	   comment-column))))

(defun octave-indent-for-comment ()
  "If there is no comment already on this line, create a code-level
comment (started by two comment characters) if the line is empty, or an
in-line comment (started by one comment character) otherwise.
Point is left after the start of the comment which is properly aligned."
  (interactive)
  (indent-for-comment)
  (indent-according-to-mode))

(defun octave-indent-line (&optional arg)
  "Indent current line as Octave code.
With optional ARG, use this as offset unless this line is a comment with
fixed goal column."
  (interactive)
  (or arg (setq arg 0))
  (let ((icol (calculate-octave-indent))
	(relpos (- (current-column) (current-indentation))))
    (if (listp icol)
	(setq icol (car icol))
      (setq icol (+ icol arg)))
    (if (< icol 0)
	(error "Unmatched end keyword")
      (indent-line-to icol)
      (if (> relpos 0)
	  (move-to-column (+ icol relpos))))))

(defun octave-indent-new-comment-line ()
  "Break line at point and indent,  continuing comment if within one and
signalling an error if within a string.  Otherwise (if within code),
insert `octave-continuation-string' before breaking the line."
  (interactive)
  (delete-horizontal-space)
  (cond
   ((octave-in-comment-p)
    (indent-new-comment-line))
   ((octave-in-string-p)
    (error "Cannot split a code line inside a string"))
   (t
    (insert (concat " " octave-continuation-string))
    (octave-reindent-then-newline-and-indent))))

(defun octave-indent-defun ()
  "Properly indents the Octave function which contains point."
  (interactive)
  (save-excursion
    (octave-mark-defun)
    (message "Indenting function...")
    (indent-region (point) (mark) nil))
  (message "Indenting function...done."))


;;; Motion
(defun octave-next-code-line (&optional arg)
  "Move ARG lines of Octave code forward (backward if ARG is negative),
skipping past all empty and comment lines; default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc))
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))
      
(defun octave-previous-code-line (&optional arg)
  "Move ARG lines of Octave code backward (forward if ARG is negative),
skipping past all empty and comment lines; default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (octave-next-code-line (- arg)))

(defun octave-beginning-of-line ()
  "If on an empty or comment line, go to the beginning of that line.
Otherwise, move backward to the beginning of the first Octave code line
which is not inside a continuation statement, i.e., which does not
follow a code line ending in `...' or `\\', or is inside an open
parenthesis list."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\s-*\\($\\|\\s<\\)"))
      (while (or (condition-case nil
		     (progn
		       (up-list -1)
		       (beginning-of-line)
		       t)
		   (error nil))
		 (and (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (save-excursion
			    (if (zerop (octave-previous-code-line))
				(looking-at octave-continuation-regexp))))
		      (zerop (forward-line -1)))))))

(defun octave-end-of-line ()
  "If on an empty or comment line, go to the end of that line.
Otherwise, move forward to the end of the first Octave code line which
does not end in `...' or `\\' or is inside an open parenthesis list."
  (interactive)
  (end-of-line)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\($\\|\\s<\\)"))
      ()
    (while (or (condition-case nil
		   (progn
		     (up-list 1)
		     (end-of-line)
		     t)
		 (error nil))
	       (and (save-excursion
		      (beginning-of-line)
		      (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (looking-at octave-continuation-regexp)))
		    (zerop (forward-line 1)))))
    (end-of-line)))
  
(defun octave-scan-blocks (from count depth)
  "Scan from character number FROM by COUNT begin-end blocks.
Returns the character number of the position thus found.

If DEPTH is nonzero, block depth begins counting from that value.
Only places where the depth in blocks becomes zero are candidates for
stopping; COUNT such places are counted.

If the beginning or end of the buffer is reached and the depth is wrong,
an error is signaled."
  (let ((min-depth (if (> depth 0) 0 depth))
	(inc (if (> count 0) 1 -1)))
    (save-excursion
      (while (/= count 0)
	(catch 'foo
	  (while (or (re-search-forward
		      octave-block-begin-or-end-regexp nil 'move inc)
		     (if (/= depth 0)
			 (error "Unbalanced block")))
	    (if (octave-not-in-string-or-comment-p)
		(progn
		  (cond
		   ((match-end 1)
		    (setq depth (+ depth inc)))
		   ((match-end 2)
		    (setq depth (- depth inc))))
		  (if (< depth min-depth)
		      (error "Containing expression ends prematurely"))
		  (if (= depth 0)
		      (throw 'foo nil))))))
	(setq count (- count inc)))
      (point))))

(defun octave-forward-block (&optional arg)
  "Move forward across one balanced begin-end block of Octave code.
With argument, do it that many times.
Negative arg -N means move backward across N blocks."
  (interactive "p")
  (or arg (setq arg 1))
  (goto-char (or (octave-scan-blocks (point) arg 0) (buffer-end arg))))

(defun octave-backward-block (&optional arg)
  "Move backward across one balanced begin-end block of Octave code.
With argument, do it that many times.
Negative arg -N means move forward across N blocks."
  (interactive "p")
  (or arg (setq arg 1))
  (octave-forward-block (- arg)))

(defun octave-down-block (arg)
  "Move forward down one begin-end block level of Octave code.
With argument, do this that many times.
A negative argument means move backward but still go down a level.
In Lisp programs, an argument is required."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (octave-scan-blocks (point) inc -1)
		     (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun octave-backward-up-block (arg)
  "Move backward out of one begin-end block level of Octave code.
With argument, do this that many times.
A negative argument means move forward but still to a less deep spot.
In Lisp programs, an argument is required."
  (interactive "p")
  (octave-up-block (- arg)))

(defun octave-up-block (arg)
  "Move forward out of one begin-end block level of Octave code.
With argument, do this that many times.
A negative argument means move backward but still to a less deep spot.
In Lisp programs, an argument is required."
  (interactive "p")
  (let ((inc (if (> arg 0) 1 -1)))
    (while (/= arg 0)
      (goto-char (or (octave-scan-blocks (point) inc 1)
		     (buffer-end arg)))
      (setq arg (- arg inc)))))

(defun octave-mark-block ()
  "Put point at the beginning of this block, mark at the end.  The block
marked is the one that contains point or follows point."
  (interactive)
  (let ((pos (point)))
    (if (or (and (octave-in-block-p)
		 (skip-syntax-forward "\\w"))
	    (condition-case nil
		(progn
		  (octave-down-block 1)
		  (octave-in-block-p))
	      (error nil)))
	(progn
	  (octave-up-block -1)
	  (push-mark (point))
	  (octave-forward-block)
	  (exchange-point-and-mark))
      (goto-char pos)
      (message "No block to mark found"))))

(defun octave-close-block ()
  "Close the current block on a separate line.  An error is signaled if
no block to close is found."
  (interactive)
  (let (bb-keyword)
    (condition-case nil
	(progn
	  (save-excursion
	    (octave-backward-up-block 1)
	    (setq bb-keyword (buffer-substring-no-properties
			      (match-beginning 1) (match-end 1))))
	  (if (save-excursion
		(beginning-of-line)
		(looking-at "^\\s-*$"))
	      (indent-according-to-mode)
	    (octave-reindent-then-newline-and-indent))
	  (insert (car (reverse
			(assoc bb-keyword
			       octave-block-match-alist))))
	  (octave-reindent-then-newline-and-indent)
	  t)
      (error (message "No block to close found")))))

(defun octave-blink-matching-block-open ()
  "If point is right after an else or end type block keyword, move
cursor momentarily to the corresponding begin keyword.
Signal an error if the keywords are incompatible."
  (interactive)
  (let (bb-keyword bb-arg eb-keyword pos eol)
    (if (and (octave-not-in-string-or-comment-p)
	     (looking-at "\\>")
	     (save-excursion
	       (skip-syntax-backward "\\w")
	       (looking-at octave-block-else-or-end-regexp)))
	(save-excursion
	  (cond
	   ((match-end 1)
	    (setq eb-keyword
		  (buffer-substring-no-properties
		   (match-beginning 1) (match-end 1)))
	    (octave-backward-up-block 1))
	   ((match-end 2)
	    (setq eb-keyword
		  (buffer-substring-no-properties
		   (match-beginning 2) (match-end 2)))
	    (octave-backward-block)))
	  (setq pos (match-end 0)
		bb-keyword
		(buffer-substring-no-properties
		 (match-beginning 0) pos)
		pos (+ pos 1)
		eol (octave-point 'eol)
		bb-arg
		(save-excursion
		  (save-restriction
		    (goto-char pos)
		    (while (and (skip-syntax-forward "^\\s<" eol)
				(octave-in-string-p)
				(not (forward-char 1))))
		    (skip-syntax-backward "\\s-")
		    (buffer-substring-no-properties pos (point)))))
	  (if (member eb-keyword
		      (cdr (assoc bb-keyword octave-block-match-alist)))
	      (progn
		(message "Matches `%s %s'" bb-keyword bb-arg)
		(if (pos-visible-in-window-p)
		    (sit-for blink-matching-delay)))
	    (error "Block keywords `%s' and `%s' do not match"
		   bb-keyword eb-keyword))))))

(defun octave-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a function.  With argument, do it
that many times.  Negative arg -N means move forward to Nth following
beginning of a function.
Returns t unless search stops at the beginning or end of the buffer."
  (interactive "p")
  (let* ((arg (or arg 1))
	 (inc (if (> arg 0) 1 -1))
	 (found))
    (and (not (eobp))
	 (not (and (> arg 0) (looking-at "\\<function\\>")))
	 (skip-syntax-forward "\\w"))
    (while (and (/= arg 0)
		(setq found
		      (re-search-backward "\\<function\\>" nil 'move inc)))
      (if (octave-not-in-string-or-comment-p)
	  (setq arg (- arg inc))))
    (if found
	(progn
	  (and (< inc 0) (goto-char (match-beginning 0)))
	  t))))

(defun octave-end-of-defun (&optional arg)
  "Move forward to the end of a function.  With positive ARG, do it that
many times.  Negative argument -N means move back to Nth preceding end
of a function.

An end of a function occurs right after the end keyword matching the
`function' keyword that starts the function."
  (interactive "p")
  (or arg (setq arg 1))
  (and (< arg 0) (skip-syntax-backward "\\w"))
  (and (> arg 0) (skip-syntax-forward "\\w"))  
  (if (octave-in-defun-p)
      (setq arg (- arg 1)))
  (if (= arg 0) (setq arg -1))
  (if (octave-beginning-of-defun (- arg))
      (octave-forward-block)))

(defun octave-mark-defun ()
  "Put point at the beginning of this function, mark at its end.  The
function marked is the one containing point or following point."
  (interactive)
  (let ((pos (point)))
    (if (or (octave-in-defun-p)
	    (and (octave-beginning-of-defun -1)
		 (octave-in-defun-p)))
	(progn
	  (skip-syntax-forward "\\w")
	  (octave-beginning-of-defun)
	  (push-mark (point))
	  (octave-end-of-defun)
	  (exchange-point-and-mark))
      (goto-char pos)
      (message "No function to mark found"))))
  

;;; Filling
(defun octave-auto-fill-mode (arg)
  "Toggle octave-auto-fill mode.
With ARG, turn `octave-auto-fill' mode on iff ARG is positive.
In `octave-auto-fill' mode, inserting a space at a column beyond
`octave-fill-column' automatically breaks the line at a previous space,
inserting `octave-continuation-string' at the end of code lines."
  (interactive "P")
  (prog1 (setq auto-fill-function
               (if (if (null arg)
                       (not auto-fill-function)
                     (> (prefix-numeric-value arg) 0))
                   'octave-auto-fill
                 nil))
    (force-mode-line-update)))

(defadvice auto-fill-mode (around octave-auto-fill activate)
  "If the current buffer is in Octave mode, toggle Octave-Auto-Fill mode.  
In this mode, inserting a space at a column beyond `octave-fill-column'
automatically breaks the line at a previous space, inserting
`octave-continuation-string' at the end of code lines."
  (if (eq major-mode 'octave-mode)
      (octave-auto-fill-mode arg)
    ad-do-it))

(defun octave-auto-fill ()
  "Function to perform auto-fill in Octave mode."
  (if (> (current-column) (current-fill-column))
      (if (octave-in-comment-p)
	  (do-auto-fill)
	;; Try to remove continuation regexps before point before
	;; inserting another one.
	;; Need to think more about this ...
;;	(let ((bol (octave-point 'bol)))
;;	  (while (re-search-backward "\\\\\\|\\.\\.\\." bol t)
;;	    (if (and (octave-not-in-string-or-comment-p)
;;		     (> (point) bol))
;;		(progn
;;		  (delete-region (match-beginning 0) (match-end 0))
;;		  (fixup-whitespace)))))
	(if (> (current-column) (current-fill-column))
	    (let ((fill-column (- (current-fill-column)
				  (length octave-continuation-string))))
	      (do-auto-fill)
	      (save-excursion
		(forward-line -1)
		(end-of-line)
		(insert (concat " " octave-continuation-string)))
	      (indent-according-to-mode))))))

(defun octave-fill-paragraph (&optional arg)
 "Fills paragraph, handling Octave comments."
 (interactive "P")
 (save-excursion 
   (let ((end (progn (forward-paragraph) (point)))
	 (beg (progn
		(forward-paragraph -1)
		(skip-chars-forward " \t\n")
		(beginning-of-line)
		(point)))
	 (cfc (current-fill-column))
	 (ind (calculate-octave-indent))
	 comment-prefix)
     (save-restriction
       (goto-char beg)
       (narrow-to-region beg end)
       (if (listp ind) (setq ind (nth 1 ind)))
       (while (not (eobp))
	 (condition-case nil
	     (octave-indent-line ind)
	   (error nil))
	 (if (and (> ind 0)
		  (not
		   (save-excursion
		     (beginning-of-line)
		     (looking-at "^\\s-*\\($\\|\\s<+\\)"))))
	     (setq ind 0))
	 (move-to-column cfc)
	 ;; First check whether we need to combine non-empty comment lines
	 (if (and (< (current-column) cfc)
		  (octave-in-comment-p)
		  (not (save-excursion
			 (beginning-of-line)
			 (looking-at "^\\s-*\\s<+\\s-*$"))))
	     ;; This is a nonempty comment line which does not extend
	     ;; past the fill column.  If it is followed by an nonempty
	     ;; comment line with the same comment prefix, try to
	     ;; combine them, and repeat this until either we reach the
	     ;; fill-column or there is nothing more to combine.
	     (progn
	       ;; Get the comment prefix
	       (save-excursion
		 (beginning-of-line)
		 (while (and (re-search-forward "\\s<+")
			     (not (octave-in-comment-p))))
		 (setq comment-prefix (match-string 0)))
	       ;; And keep combining ...
	       (while (and (< (current-column) cfc)
			   (save-excursion
			     (forward-line 1)
			     (and (looking-at
				   (concat "^\\s-*"
					   comment-prefix
					   "\\S<"))
				  (not (looking-at
					(concat "^\\s-*"
						comment-prefix
						"\\s-*$"))))))
		 (delete-char 1)
		 (re-search-forward comment-prefix)
		 (delete-region (match-beginning 0) (match-end 0))
		 (fixup-whitespace)
		 (move-to-column cfc))))
	 (skip-chars-forward "^ \t\n")
	 (delete-horizontal-space)
	 (if (or (< (current-column) cfc)
		 (and (= (current-column) cfc) (eolp)))
	     (forward-line 1)
	   (if (not (eolp)) (insert " "))
	   (octave-auto-fill))))
     t)))


;;; Completions
(defun octave-initialize-completions ()
  "Create an alist for completions."
  (if octave-completion-alist
      ()
    (setq octave-completion-alist
	  (mapcar '(lambda (var) (cons var var))
		  (append octave-reserved-words
			  octave-text-functions
			  octave-variables)))))

(defun octave-complete-symbol ()
  "Perform completion on Octave symbol preceding point, comparing that
symbol against Octave's reserved words and builtin variables."
  ;; This code taken from lisp-complete-symbol
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion (backward-sexp 1) (point)))
	 (string (buffer-substring-no-properties beg end))
	 (completion (try-completion string octave-completion-alist)))
    (cond ((eq completion t))		; ???
	  ((null completion)
	   (message "Can't find completion for \"%s\" pattern")
	   (ding))
	  ((not (string= string completion))
           (delete-region beg end)
           (insert completion))
	  (t
	   (let ((list (all-completions string octave-completion-alist))
		 (conf (current-window-configuration)))
	     ;; Taken from comint.el
	     (message "Making completion list...")	       
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list))
	     (message "Hit space to flush")
	     (let (key first)
	       (if (save-excursion
		     (set-buffer (get-buffer "*Completions*"))
		     (setq key (read-key-sequence nil)
			   first (aref key 0))
		     (and (consp first) (consp (event-start first))
			  (eq (window-buffer (posn-window (event-start
							   first)))
			      (get-buffer "*Completions*"))
			  (eq (key-binding key) 'mouse-choose-completion)))
		   (progn
		     (mouse-choose-completion first)
		     (set-window-configuration conf))
		 (if (eq first ?\ )
		     (set-window-configuration conf)
		   (setq unread-command-events
			 (listify-key-sequence key))))))))))
	       

;;; Electric characters && friends
(defun octave-reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line.
If Abbrev mode is on, expand abbrevs first."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (if octave-blink-matching-block
      (octave-blink-matching-block-open))
  (save-excursion
    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
    (indent-according-to-mode))
  (insert "\n")
  (indent-according-to-mode))

(defun octave-electric-semi ()
  "Insert `;' character and reindent the line.  Insert a newline if
`octave-auto-newline' is non-nil."
  (interactive)
  (if (not (octave-not-in-string-or-comment-p))
      (insert ";")
    (if abbrev-mode (expand-abbrev))
    (if octave-blink-matching-block
	(octave-blink-matching-block-open))
    (indent-according-to-mode)    
    (insert ";")
    (if octave-auto-newline
	(newline-and-indent))))

(defun octave-electric-space ()
  "Maybe expand abbrevs and blink matching block open keywords, and
insert a space."
  (interactive)
  (setq last-command-char ? )
  (if (not (octave-not-in-string-or-comment-p))
      (progn
	(indent-according-to-mode)
	(self-insert-command 1))
    (if abbrev-mode (expand-abbrev))
    (if octave-blink-matching-block
	(octave-blink-matching-block-open))
    (if (save-excursion
	  (skip-syntax-backward "\\s-")
	  (not (bolp)))
	(indent-according-to-mode))
    (self-insert-command 1)))

(defun octave-insert-defun (name args vals)
  "Insert a function skeleton, prompting for the function's name,
arguments and return values (to be entered without parens)."
  (interactive
   (list
    (read-from-minibuffer "Function name: "
			  (substring (buffer-name) 0 -2))
    (read-from-minibuffer "Arguments: ")
    (read-from-minibuffer "Return values: ")))
  (let ((string (format "%s %s (%s)"
			(cond
			 ((string-equal vals "")
			  vals)
			 ((string-match "[ ,]" vals)
			  (concat " [" vals "] ="))
			 (t
			  (concat " " vals " =")))
			name
			args))
	(prefix octave-block-comment-start))
    (if (not (bobp)) (newline))
    (insert "function" string)
    (indent-according-to-mode)
    (newline 2)
    (insert prefix "usage: " string)
    (reindent-then-newline-and-indent)    
    (insert prefix)
    (reindent-then-newline-and-indent)    
    (insert prefix)
    (indent-according-to-mode)
    (save-excursion
      (newline 2)
      (insert "endfunction")
      (indent-according-to-mode))))
	      

;;; Menu
(defun octave-add-octave-menu ()
  "Adds the `Octave' menu to the menu bar in Octave mode."
  (require 'easymenu)  
  (easy-menu-define octave-mode-menu-map octave-mode-map
		    "Menu keymap for Octave mode." octave-mode-menu)
  (easy-menu-add octave-mode-menu-map octave-mode-map))


;;; Help
(require 'info)

(defun octave-describe-major-mode ()
  "Describe the current major mode."
  (interactive)
  (describe-function major-mode))

(defun octave-help (key)
  "Look up KEY in the function, operator and variable indices of the
info files with documentation for Octave.
If KEY is not a string, prompt for it with completion."
  (interactive
   (list
    (completing-read (format "Describe Octave entity: ")
		     (octave-help-get-completion-alist)
		     nil t)))
  (setq octave-help-index-alternatives nil)
  (if (get-buffer "*info*")
      (set-buffer "*info*"))
  (if (zerop (length key))
      (Info-find-node (car octave-help-files) "Top")
    (let ((alist (copy-alist (octave-help-get-lookup-alist)))
	  entry matches)
      (while (setq entry (car alist))
	(if (string-match key (car entry))
	    (add-to-list 'matches entry))
	(setq alist (cdr alist)))
;;      (while (setq entry (assoc key alist))
;;	(add-to-list 'matches entry)
;;	(delete entry alist))
      (if matches
	  (progn
	    (setq octave-help-index-alternatives matches
		  Info-index-alternatives matches)
	    (Info-index-next 0))))))

;; The following three advices are really ugly.  The problem is that
;; Info-index-next can only cycle through matches found in ONE info
;; file, because whenever it changes the info file, it does so using
;; Info-find-node which resets Info-index-alternatives to nil.  Hence,
;; we save the current value before and restore it after calling this
;; function when necessary.
    
(defadvice Info-find-node (before octave-help activate)
  (and Info-index-alternatives
       octave-help-index-alternatives
       (setq octave-help-index-alternatives Info-index-alternatives)))

(defadvice Info-find-node (after octave-help activate)
  (and octave-help-index-alternatives
       (not Info-index-alternatives)
       (setq Info-index-alternatives octave-help-index-alternatives)))

(defadvice Info-exit (before octave-help activate)
  (setq octave-help-index-alternatives nil))

(defun octave-help-get-lookup-alist ()
  "Build the index lookup alist from all info files with documentation
for Octave."
  (if octave-help-lookup-alist
      ()
    (message "Building help lookup alist...")    
    (let ((files octave-help-files) file key node)
      (save-window-excursion
	(while files
	  (setq file (car files))
 	  (Info-goto-node (concat "(" file ")"))
	  (condition-case nil
	      (progn
		(Info-index "")
		(while
		    (progn
		      (while (re-search-forward
			      "^\\* \\([^(:]+\\)[^:]*: *\\(.+\\)\\.$"
			      nil t)
			(setq key (match-string 1)
			      node (concat "(" file ")" (match-string 2)))
			(and (string-match "\\(.*\\>\\) *$" key)
			     (setq key (replace-match "\\1" t nil key)))
			(add-to-list 'octave-help-lookup-alist
				     (list key
					   node
					   (concat (concat "(" file ")")
						   Info-current-node)
					   0)))
		      (and (setq node (Info-extract-pointer "next" t))
			   (string-match
			    (concat "\\(Function\\|Operator\\|Variable\\) "
				    "\\<Index\\>")
			    node)))
		  (Info-goto-node node)))
	    (error nil))
	  (setq files (cdr files)))))
    (message "Building help lookup alist...done"))
  octave-help-lookup-alist)

(defun octave-help-get-completion-alist ()
  "Build the index completion alist from all info files with
documentation for Octave."
  (if octave-help-completion-alist
      ()
    (message "Building help completion alist...")
    (let ((alist (octave-help-get-lookup-alist)) entry)
      (while alist
	(setq entry (car alist))
	(add-to-list 'octave-help-completion-alist
		     (cons (car entry) (car entry)))
	(setq alist (cdr alist))))
    (message "Building help completion alist...done"))    
  octave-help-completion-alist)


;;; Inferior Octave mode
(require 'comint)

(defvar inferior-octave-buffer "*Inferior Octave*"
  "*Name of buffer for running an inferior Octave process")

(defvar inferior-octave-process nil)

(defvar inferior-octave-program "octave")

(defvar inferior-octave-prompt "\\(^octave\\(:[0-9]+\\)?\\|^\\)>+ "
  "*Regexp to match prompts for the inferior Octave process")

(defvar inferior-octave-mode-map nil)
(if inferior-octave-mode-map
    ()
  (let ((map (copy-keymap comint-mode-map)))
    ;; (define-key map "\t" 'inferior-octave-complete)
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-filename-completions)
    (setq inferior-octave-mode-map map)))
    
(defvar inferior-octave-receive-in-progress nil)
(defvar inferior-octave-output-list nil)
(defvar inferior-octave-output-string nil)

(defvar inferior-octave-startup-file nil
  "*Name of a file to send the contents of to the inferior Octave process
on startup.")

(defvar inferior-octave-startup-args nil
  "*A list of command line arguments to be passed to the inferior Octave
process on startup.
For example, for suppressing the startup message and using `traditional'
mode, set this to '(\"-q\" \"--traditional\").")

(defvar inferior-octave-font-lock-keywords
  (list
   (cons inferior-octave-prompt 'font-lock-type-face))
  "Additional expressions to highlight in Inferior Octave mode.")

(defvar inferior-octave-dynamic-complete-functions
  '(inferior-octave-complete comint-dynamic-complete-filename)  
  "List of functions called to perform completion.
This variable is used to initialise `comint-dynamic-complete-functions'
in the Inferior Octave buffer.")

(defvar inferior-octave-mode-hook nil
  "*Hook to be run when Inferior Octave mode is started.")

(defun inferior-octave-mode ()
  "Major mode for interacting with an inferior Octave process.
Runs Octave as a subprocess of Emacs, with Octave I/O through an Emacs
buffer.

Entry to this mode first runs the hook `comint-mode-hook'.  Then, it
sends a few initialization commands to the Octave process.  Additional
commands to be executed on startup can be provided either in the file
specified by `inferior-octave-startup-file' or by the default startup
file, \"~/.emacs_octave\".  Executing these commands will NOT produce
any visible output.

Finally. `inferior-octave-mode-hook' is run."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-octave-prompt
	major-mode 'inferior-octave-mode
	mode-name "Inferior Octave"
	mode-line-process '(":%s"))
  (use-local-map inferior-octave-mode-map)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'inferior-octave-output-list)
  (make-local-variable 'inferior-octave-receive-in-progress)  
  (setq comint-input-ring-file-name
	(or (getenv "OCTAVE_HISTFILE") "~/.octave_hist")
	comint-input-ring-size
	(or (getenv "OCTAVE_HISTSIZE") 1024)
	comint-dynamic-complete-functions
	inferior-octave-dynamic-complete-functions
	font-lock-defaults
	'(inferior-octave-font-lock-keywords nil nil)
	inferior-octave-process
	(get-buffer-process inferior-octave-buffer)
	inferior-octave-output-list nil
	inferior-octave-receive-in-progress t
	comint-input-filter-functions '(inferior-octave-directory-tracker))
  (set-process-filter inferior-octave-process
		      'inferior-octave-slurp-output)
  (while inferior-octave-receive-in-progress
    (accept-process-output inferior-octave-process))
  (if (not (bobp))
      (insert-before-markers "\n"))
  (if inferior-octave-output-list
      (insert-before-markers
       (concat (mapconcat 'identity
			  inferior-octave-output-list "\n")
	       "\n")))
  ;; Inferior Octave startup commands
  (let* ((commands '("page_screen_output = 0;\n"))
	 (program (file-name-nondirectory inferior-octave-program))	
	 (startfile (or inferior-octave-startup-file
			(concat "~/.emacs_" program))))
    (if (not (string-equal inferior-octave-output-string ">> "))
	(add-to-list 'commands "PS1=\"\\\\s> \";\n"))
    (if (file-exists-p startfile)
	(add-to-list 'commands
		     (format "source (\"%s\");\n" startfile)))
    (inferior-octave-send-list-slurp-output commands))
  (set-process-filter inferior-octave-process
		      'inferior-octave-output-filter)
  (run-hooks 'inferior-octave-mode-hook)
  (insert-before-markers inferior-octave-output-string)  
  (comint-read-input-ring t))

;;;###autoload
(defun inferior-octave (&optional arg)
  "Run an inferior Octave process, with I/O through the buffer specified
by `inferior-octave-buffer'.  The buffer is put in Inferior Octave mode.
See `inferior-octave-mode'.

Unless ARG is non-nil, switches to this buffer.

The elements of the list `inferior-octave-startup-args' are sent as
command line arguments to the Octave process on startup."
  (interactive "P")
  (if (comint-check-proc inferior-octave-buffer)
      ()
    (save-excursion
      (set-buffer
       (apply 'make-comint
	      (substring inferior-octave-buffer 1 -1)
	      (file-name-nondirectory inferior-octave-program)
	      nil
	      inferior-octave-startup-args))
      (inferior-octave-mode)))
  (if (not arg)
      (pop-to-buffer inferior-octave-buffer)))

;;;###autoload
(defalias 'run-octave 'inferior-octave)

(defun inferior-octave-complete ()
  "Perform completion on the Octave command preceding point.
This is implemented using the Octave command `completion_matches' which
is NOT available with older versions of Octave."
  (interactive)
  (let* ((end (point))
	 (command (save-excursion
		    (skip-syntax-backward "\\w\\s_")
		    (and (looking-at comint-prompt-regexp)
			 (goto-char (match-end 0)))
		    (buffer-substring-no-properties (point) end)))
	 (proc (get-buffer-process inferior-octave-buffer))
	 (filter (process-filter proc)))
    (if (string-equal command "")
	(message "Cannot complete an empty string")
      (setq inferior-octave-output-list nil
	    inferior-octave-receive-in-progress t)
      (set-process-filter proc 'inferior-octave-slurp-output)
      (unwind-protect
	  (progn
	    (comint-send-string proc (concat "completion_matches (\""
					     command "\");\n"))
	    (while inferior-octave-receive-in-progress
	      (accept-process-output proc)))
	(set-process-filter proc filter))
      (and inferior-octave-output-list
	   (string-match "^\a*error:"
			 (car inferior-octave-output-list))
	   (error (concat "This version of Octave does not support the "
			  "`completion_matches' command")))
      ;; Sort the list
      (setq inferior-octave-output-list
	    (sort inferior-octave-output-list 'string-lessp))
      ;; Remove duplicates
      (let* ((x inferior-octave-output-list)
	     (y (cdr x)))
	(while y
	  (if (string-equal (car x) (car y))
	      (setcdr x (setq y (cdr y)))
	    (setq x y
		  y (cdr y)))))
      ;; And let comint handle the rest
      (comint-dynamic-simple-complete command
				      inferior-octave-output-list))))

(defun octave-strip-ctrl-g (string)
  "If STRING starts with a `^G', ring the bell and strip it."
  (if (string-match "^\a" string)
      (progn
        (ding)
        (setq string (substring string 1))))
  string)

(defun inferior-octave-output-filter (proc string)
  "Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to comint-output-filter."
  (comint-output-filter proc (octave-strip-ctrl-g string)))

(defun inferior-octave-slurp-output (proc string)
  "Save all output from the inferior Octave process between newlines into
inferior-octave-output-list, the rest to inferior-octave-output-string."
  (while (string-match "\n" string)
    (setq inferior-octave-output-list
	  (append inferior-octave-output-list
		  (list (substring string 0 (match-beginning 0))))
	  string (substring string (match-end 0))))
  (if (string-match inferior-octave-prompt string)
      (setq inferior-octave-output-string string
	    inferior-octave-receive-in-progress nil)))

(defun inferior-octave-send-list-slurp-output (list)
  "Send the elements of LIST (which have to be strings) to the Octave
process, passing all output to the filter inferior-octave-slurp-output."
  (let* ((proc (get-buffer-process inferior-octave-buffer))
	 (filter (process-filter proc))
	 string)
    (set-process-filter proc 'inferior-octave-slurp-output)
    (setq inferior-octave-output-list nil)
    (unwind-protect
	(while (setq string (car list))
	  (setq inferior-octave-receive-in-progress t)
	  (comint-send-string proc string)
	  (while inferior-octave-receive-in-progress
	    (accept-process-output proc))
	  (setq list (cdr list)))
      (set-process-filter proc filter))))

(defun inferior-octave-directory-tracker (string)
  "Tracks cd commands issued to Octave.
Use M-x inferior-octave-resync-dirs to resync if Emacs gets confused."
  (if (string-match "[ \t]*cd[ \t]*\\([^ \t\n;]*\\)[ \t\n;]"
		    string)
      (cd (substring string (match-beginning 1) (match-end 1)))))

(defun inferior-octave-resync-dirs ()
  "Resync the buffer's idea of the current directory."
  (interactive)
  (inferior-octave-send-list-slurp-output '("pwd\n"))
  (cd (car inferior-octave-output-list)))


;;; Communication with the inferior Octave process
(defvar octave-send-show-buffer t
  "*Non-nil means always display inferior-octave-buffer after sending a
command.")
(defvar octave-send-line-auto-forward t
  "*Non-nil means always go to the next line of Octave code after
sending a line to the inferior process.")
(defvar octave-send-echo-input t
  "*Non-nil means echo input sent to the inferior Octave process.")

(defun octave-kill-process ()
  "Kill inferior Octave process and its buffer."
  (interactive)
  (if inferior-octave-process
      (comint-send-string inferior-octave-process "quit;\n"))
  (if inferior-octave-buffer
      (kill-buffer inferior-octave-buffer)))

(defun octave-show-process-buffer ()
  "Make sure that `inferior-octave-buffer' is displayed."
  (interactive)
  (display-buffer inferior-octave-buffer))

(defun octave-hide-process-buffer ()
  "Delete all windows that display `inferior-octave-buffer'."
  (interactive)
  (delete-windows-on inferior-octave-buffer))

(defun octave-send-region (beg end)
  "Send the region to the inferior Octave process."
  (interactive "r")
  (if (not (comint-check-proc inferior-octave-buffer))
      (inferior-octave t))
  (let ((proc inferior-octave-process)
	(string (buffer-substring beg end))
	line)
    (save-excursion
      (set-buffer inferior-octave-buffer)
      (setq inferior-octave-output-list nil)
      (while (not (string-equal string ""))
	(if (string-match "\n" string)
	    (setq line (substring string 0 (match-beginning 0))
		  string (substring string (match-end 0)))
	  (setq line string string ""))
	(setq inferior-octave-receive-in-progress t)
	(inferior-octave-send-list-slurp-output
	 (list (concat line "\n")))
	(while inferior-octave-receive-in-progress
	  (accept-process-output proc))
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if octave-send-echo-input (list line) (list ""))
		     (mapcar 'octave-strip-ctrl-g
			     inferior-octave-output-list)
		     (list inferior-octave-output-string))
		    "\n")))))
  (if octave-send-show-buffer
      (display-buffer inferior-octave-buffer)))

(defun octave-send-block ()
  "Send the current block to the inferior Octave process."  
  (interactive)
  (save-excursion
    (octave-mark-block)
    (octave-send-region (point) (mark))))

(defun octave-send-defun ()
  "Send the current function to the inferior Octave process."
  (interactive)
  (save-excursion
    (octave-mark-defun)
    (octave-send-region (point) (mark))))

(defun octave-send-line (&optional arg)
  "Send the current code line to the inferior Octave process.
With positive prefix ARG, send that many lines.
If `octave-send-line-auto-forward' is non-nil, go to the next unsent
code line."
  (interactive "P")
  (or arg (setq arg 1))
  (if (> arg 0)
      (let (beg end)
	(beginning-of-line)
	(setq beg (point))
	(octave-next-code-line (- arg 1))
	(end-of-line)
	(setq end (point))
	(if octave-send-line-auto-forward
	    (octave-next-code-line 1))
	(octave-send-region beg end))))

(defun octave-eval-print-last-sexp ()
  "Evaluate Octave sexp before point; print value into current buffer."
  (interactive)
  (let ((standard-output (current-buffer))
	(print-escape-newlines nil)
	(opoint (point)))
    (terpri)
    (prin1    
     (save-excursion
       (forward-sexp -1)
       (inferior-octave-send-list-slurp-output
	(list (concat (buffer-substring-no-properties (point) opoint)
		      "\n")))
       (mapconcat 'identity inferior-octave-output-list "\n")))
    (terpri)))
    

;;; Bug reporting
(defun octave-submit-bug-report ()
  "Submit a bug report on Octave mode via mail."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a bug report? ")
   (reporter-submit-bug-report
    octave-help-address
    (concat "Octave version " octave-version)
    (list
     'octave-auto-newline
     'octave-blink-matching-block
     'octave-block-offset
     'octave-comment-char
     'octave-comment-column
     'octave-continuation-offset
     'octave-continuation-string
     'octave-fill-column
     'octave-inhibit-startup-message
     'octave-help-files
     'octave-send-echo-input
     'octave-send-line-auto-forward
     'octave-send-show-buffer))))

;;; provide ourself

(provide 'octave)

;; Compile this file when saving it:

;;; Local Variables:
;;; after-save-hook: ((lambda () (byte-compile-file buffer-file-name)))
;;; End:
