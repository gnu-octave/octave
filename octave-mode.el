;; octave-mode.el - major mode for editing Octave source with GNU Emacs
;;
;; This major mode for GNU Emacs provides support for editing Octave
;; source files.  It automatically indents for block structures, line
;; continuations (e.g., ...), and comments.  The usual paren matching
;; support is included.  Indenting for continued matrix expressions is
;; currently not supported.  Perhaps it will be in the future.  Auto-fill
;; mode seems to actually work!  For convient use add something like the
;; following to your .emacs start-up file:
;;
;;   (autoload 'octave-mode "octave-mode" "Enter Octave-mode." t)
;;   (setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
;;   (setq octave-mode-hook '(lambda () (setq fill-column 74)))
;;
;; Enjoy.
;;
;; Last modified Sun Mar  7 17:55:20 1993.
;;
;; This file was modified by John W. Eaton (jwe@che.utexas.edu) from
;; the file matlab-mode.el which is:
;;
;; Copyright (C) 1991 Matthew R. Wette.
;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made the last modification and the date of such modification.
;;   3. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.
;;
;; Version 1.01, dated 25Jan91
;;
;; 25Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Got indentation of matrix expression to work, I think.  Also,
;;	added tabs to comment start regular-expression.
;;
;; 14Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Added functions (ml-unbal-matexp ml-matexp-indent) for matrix
;;	expressions.
;;
;; 07Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;      Many changes.  Seems to work reasonably well.  Still would like
;;      to add some support for filling in comments and handle continued
;;      matrix expressions.  Released as Version 1.0.
;;
;; 04Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;      Created.  Used eiffel.el as a guide.


;; Constants used in all Octave-mode buffers.
(defconst octave-indent-level 2
  "*The indentation in Octave-mode.")

(defconst octave-comment-column 40
  "*The goal comment column in Octave-mode buffers.")


;; Syntax Table
(defvar octave-mode-syntax-table nil
  "Syntax table used in Octave-mode buffers.")

(if octave-mode-syntax-table
    ()
  (setq octave-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." octave-mode-syntax-table)
  (modify-syntax-entry ?/ "." octave-mode-syntax-table)
  (modify-syntax-entry ?* "." octave-mode-syntax-table)
  (modify-syntax-entry ?+ "." octave-mode-syntax-table)
  (modify-syntax-entry ?- "." octave-mode-syntax-table)
  (modify-syntax-entry ?= "." octave-mode-syntax-table)
  (modify-syntax-entry ?< "." octave-mode-syntax-table)
  (modify-syntax-entry ?> "." octave-mode-syntax-table)
  (modify-syntax-entry ?& "." octave-mode-syntax-table)
  (modify-syntax-entry ?| "." octave-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" octave-mode-syntax-table)
  (modify-syntax-entry ?# "<" octave-mode-syntax-table)
  (modify-syntax-entry ?% "<" octave-mode-syntax-table)
  (modify-syntax-entry ?\n ">" octave-mode-syntax-table)
  (set-syntax-table octave-mode-syntax-table))


;; Abbrev Table
(defvar octave-mode-abbrev-table nil
  "Abbrev table used in Octave-mode buffers.")

(define-abbrev-table 'octave-mode-abbrev-table ())


;; Mode Map
(defvar octave-mode-map ()
  "Keymap used in octave-mode.")

(if octave-mode-map
    ()
  (setq octave-mode-map (make-sparse-keymap))
  (define-key octave-mode-map "\r" 'octave-return)
  (define-key octave-mode-map "\t" 'octave-indent-line)
  (define-key octave-mode-map "\M-;" 'octave-comment)
  (define-key octave-mode-map "\C-ct" 'octave-line-type)
  (define-key octave-mode-map "\C-ci" 'octave-indent-type)
  (define-key octave-mode-map "\M-\r" 'newline))


;; Octave Mode
(defun octave-mode ()
  "Major mode for editing Octave source files.  Version 1.0, 23 Feb 1993.
Will run octave-mode-hook if it is non-nil.  Auto-fill-mode seems to work.
Filling does not work (yet).
Special Key Bindings:
\\{octave-mode-map}
Variables:
  octave-indent-level                   Level to indent blocks.
  octave-comment-column                 Goal column for on-line comments.
  fill-column                           Column used in auto-fill (default=70).
Commands:
  octave-mode                           Enter Octave major mode.
  octave-return                         Handle return with indenting.
  octave-indent-line                    Indent line for structure.
  octave-comment                        Add comment to current line.
  octave-comment-indent                 Compute indent for comment.
  octave-line-type                      Tell current line type (for debugging).
  octave-indent-type                    Tell last indent type (for debugging).
To add automatic support put something like the following in your .emacs file:
  \(autoload 'octave-mode \"octave-mode\" \"Enter Octave-mode.\" t\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.m$\" . octave-mode\) \
auto-mode-alist\)\)
  \(setq octave-mode-hook '\(lambda \(\) \(setq fill-column 74\)\)\)"
  (interactive)
  (kill-all-local-variables)
  (use-local-map octave-mode-map)
  (setq major-mode 'octave-mode)
  (setq mode-name "Octave")
  (setq local-abbrev-table octave-mode-abbrev-table)
  (set-syntax-table octave-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'octave-indent-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[%#][ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 'octave-comment-column)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'octave-comment-indent)
  (make-local-variable 'fill-column)
  (setq fill-column default-fill-column)
  (run-hooks 'octave-mode-hook))


(defun octave-return ()
  "Handle carriage return in Octave-mode."
  (interactive)
  (if (oct-block-end-line)
      (octave-indent-line))
  (newline)
  (octave-indent-line))

(defun octave-comment ()
  "Add a comment to the following line, or format if one already exists."
  (interactive)
  (cond
   ((oct-empty-line)
    (octave-indent-line)
    (insert "# "))
   ((oct-comment-line))
   (t
    (end-of-line)
    (re-search-backward "[^ \t^]" 0 t)
    (forward-char)
    (delete-horizontal-space)
    (if (< (current-column) octave-comment-column)
        (indent-to octave-comment-column)
      (insert " "))
    (insert "# "))))

(defun octave-comment-indent ()
  "Indent a comment line in Octave-mode."
  (oct-calc-indent))

(defun octave-indent-line ()
  "Indent a line in Octave-mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (oct-calc-indent)))
  (skip-chars-forward " \t"))

(defun octave-line-type ()
  "Display type of current line.  Used in debugging."
  (interactive)
  (cond
   ((oct-empty-line)
    (message "octave-line-type: empty-line"))
   ((oct-comment-line)
    (message "octave-line-type: comment-line"))
   ((oct-continuation-line)
    (message "octave-line-type: continuation-line"))
   ((oct-block-beg-end-line)
    (message "octave-line-type: block-beg-end-line"))
   ((oct-block-beg-line)
    (message "octave-line-type: block-beg-line"))
   ((oct-block-end-line)
    (message "octave-line-type: block-end-line"))
   (t
    (message "octave-line-type: other"))))

(defun octave-indent-type ()
  "Display type of current or previous nonempty line.  Used in debugging."
  (interactive)
  (message (concat "octave-ident-type: " oct-last-indent-type)))

(defun octave-fill-region (from to &optional justify-flag)
  "Fill the region of comments.
Prefix arg (non-nil third arg, if called from program)
means justify as well."
  (interactive "r\nP")
  (messages "octave-fill-region not implemented yet."))

(defvar oct-last-indent-type "unknown"
  "String to tell line type.")

(defun oct-calc-indent ()
  "Return the appropriate indentation for this line as an int."
  (let ((indent 0))
    (save-excursion
      (forward-line -1)                 ; compute indent based on previous
      (if (oct-empty-line)               ;   non-empty line
          (re-search-backward "[^ \t\n]" 0 t))
      (cond
       ((oct-empty-line) 
        (setq oct-last-indent-type "empty"))
       ((oct-comment-line) 
        (setq oct-last-indent-type "comment"))
       ((oct-continuation-line)
        (setq oct-last-indent-type "continuation")
        (setq indent (* 2 octave-indent-level)))
       ((oct-block-beg-end-line)
        (setq oct-last-indent-type "block begin-end"))
       ((oct-block-beg-line)
        (setq oct-last-indent-type "block begin")
        (setq indent octave-indent-level))
       ((oct-unbal-matexp-line)
        (setq oct-last-indent-type "unbalanced-matrix-expression")
        (setq indent (oct-calc-matexp-indent)))
       (t
        (setq oct-last-indent-type "other")))
      (setq indent (+ indent (current-indentation)))
      (if (= 0 (forward-line -1))
          (if (oct-continuation-line)
              (setq indent (- indent (* 2 octave-indent-level))))))
    (if (oct-block-end-line) (setq indent (- indent octave-indent-level)))
    (if (< indent 0) (setq indent 0))
    indent))


(defun oct-empty-line ()
  "Returns t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun oct-comment-line ()
  "Returns t if current line is an Octave comment line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "[%#]")))

(defun oct-continuation-line ()
  "Returns t if current line ends in ... and optional comment."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\.\\.\\.+[ \t]*\\(%.*\\)?$" (oct-eoln-point) t)))

(defun oct-eoln-point ()
  "Returns point for end-of-line in Octave-mode."
  (save-excursion
    (end-of-line)
    (point)))

(defun oct-block-beg-line ()
  "Returns t if line contains beginning of Octave block."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "\\([^%#\n]*[ \t]\\)?" oct-block-beg-kw))))

(defconst oct-block-beg-kw "\\(for\\|while\\|if\\|else\\|elseif\\|function\\)"
  "Regular expression for keywords which begin blocks in Octave-mode.")

(defun oct-block-end-line ()
  "Returns t if line contains end of Octave block."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "\\([^%#\n]*[ \t]\\)?" oct-block-end-kw))))

(defconst oct-block-end-kw "\\(end\\|endfor\\|endwhile\\|endif\\|endfunction\\|else\\|elseif\\)"
  "Regular expression for keywords which end blocks.")

(defun oct-block-beg-end-line ()
  "Returns t if line contains matching block begin-end in Octave-mode."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat
                 "\\([^%#\n]*[ \t]\\)?" oct-block-beg-kw 
                 "." "\\([^%#\n]*[ \t]\\)?" oct-block-end-kw))))

(defun oct-unbal-matexp-line ()
  (if (= (oct-calc-matexp-indent) 0)
    ()
    t))

(defun oct-calc-matexp-indent ()
  (let ((indent 0))
    (save-excursion
      (beginning-of-line)
      (while (< (point) (oct-eoln-point))
	(cond
	 ((looking-at "\\[")
	  (setq indent (+ indent octave-indent-level)))
	 ((looking-at "\\]")
	  (setq indent (- indent octave-indent-level))))
	(forward-char)))
    (* 2 indent)))

(defun oct-comment-on-line ()
  "Returns t if current line contains a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "[^\n]*[%#]")))

(defun oct-in-comment ()
  "Returns t if point is in a comment."
  (save-excursion
    (and (/= (point) (point-max)) (forward-char))
    (search-backward "[%#]" (save-excursion (beginning-of-line) (point)) t)))

(provide 'octave-mode)

;; --- last line of octave-mode.el --- 
