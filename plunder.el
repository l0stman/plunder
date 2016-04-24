;;; plunder.el -- minor mode for structurally editing C code.
;;; Copyright (C) 2010-2011 rrl <endian.sign@gmail.com>

;;; Repository: http://github.com/l0stman/plunder

;;; This is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your
;;; option) any later version.  This is distributed in the hope that
;;; it will be useful, but without any warranty; without even the
;;; implied warranty of merchantability or fitness for a particular
;;; purpose.  See the GNU General Public License for more details.
;;; See http://www.gnu.org/licenses/ for details.

(eval-when-compile
  (require 'cc-cmds)
  (require 'cl))

(declare-function c-newline-and-indent "cc-cmds" nil)
(declare-function c-indent-line-or-region "cc-cmds" nil)
(declare-function c-in-literal "cc-mods" (&optional lim detect-cpp))
(declare-function c-try-one-liner "cc-mods" nil)
(declare-function c-point-syntax "cc-mods" nil)
(declare-function c-brace-newlines "cc-mods" (syntax))
(declare-function c-indent-line "cc-mods"
                  (&optional syntax quiet ignore-point-pos))
(declare-function c-safe-position "cc-mods" (bufpos paren-state))
(declare-function c-parse-state "cc-mods" nil)
(declare-function c-backward-sws "cc-mods" nil)
(declare-function c-on-identifier "cc-mods" nil)
(declare-function c-beginning-of-macro "cc-mods" nil)
(declare-function c-forward-over-cpp-define-id "cc-mods" nil)
(declare-function c-indent-exp "cc-mods" nil)
(declare-function c-end-of-statement "cc-mods" nil)

(define-minor-mode plunder-mode
  "Minor mode for pseudo-structurally editing C code.
\\{plunder-mode-map}"
  :lighter " Plunder"
  :keymap '(("[" . plunder-bracket)
            ("]" . plunder-bracket)
            ("{" . plunder-electric-brace)
            ("}" . plunder-electric-brace)
            ("(" . plunder-electric-paren)
            (")" . plunder-electric-paren)
            ("\M-s" . plunder-splice-sexp)
            ("\M-r" . plunder-raise-sexp)
            ("\M-(" . plunder-wrap-sexp-in-paren)
            ("\M-[" . plunder-wrap-sexp-in-bracket)
            ("\M-{" . plunder-wrap-sexp-in-brace)
            ("\C-\M-u" . plunder-backward-up-list)))

(defun inlistp ()
  "Return true if we're inside a C list."
  (c-intersect-lists
   '(brace-list-intro brace-list-entry brace-entry-open)
   (c-save-buffer-state () (c-guess-basic-syntax))))

(defsubst newlinep (close)
  (and (eq close ?\}) (not (inlistp))))

(defun plunder-balance (close)
  "Eventually insert a newline before inserting the closing token."
  (save-excursion
    (if (newlinep close)
        (let ((p (point)))
          (insert ?\;)
          (c-newline-and-indent)
          (insert close)
          (c-indent-line-or-region)
          (goto-char p)
          (delete-char 1))
      (insert close))))

(defun plunder-backward-up-list (&optional forward-p)
  "Move backward or forward if forward-p is true out of one level
of parentheses."
  (interactive)
  (cl-flet ((matchp (l r)
                    (or (and (eq l ?\() (eq r ?\)))
                        (and (eq l ?\[) (eq r ?\]))
                        (and (eq l ?\{) (eq r ?\})))))
    (up-list)
    (let ((rtok (char-before))
          (ltok (progn (backward-sexp) (char-after))))
      (unless (matchp ltok rtok)
        (error "Mismatched tokens: %c %c." ltok rtok)))
    (when forward-p (forward-sexp))))

(defmacro liter-p () `(c-save-buffer-state () (c-in-literal)))

(defmacro re-bsearch (&rest res)
  "Search backward the concatenation of the REs given as
arguments separated by spaces.  Returns true if the beginning of
the match is not in a literal."
  `(save-excursion
     (and (re-search-backward
           ,(mapconcat #'identity res "\\(?:[ \t\n]\\|\\\\\n\\)*")
           nil t)
          (not (liter-p)))))

(defun plunder-move-past-close (close)
  "Delete the trailing blanks before the closing token and move
past it, eventually leaving a newline.  It's possible to move
past the closing token inside a nested expression."
  (interactive "*")
  (loop with rtok
        do
        (condition-case ()
            (plunder-backward-up-list t)
          (scan-error (error "Unbalanced %c." close)))
        (setq rtok (char-before))
        (let ((line-p (newlinep rtok)))
          (save-excursion
            (backward-char)
            (delete-region (point)
                           (if (re-search-backward "[^ \t\n\\]" nil t)
                               (let ((lit (liter-p)))
                                 (if (or line-p
                                         (and lit (not (eq lit 'string))))
                                     (forward-line)
                                   (forward-char))
                                 (point))
                             (point-min)))))
        until (eq rtok close)))

(defun plunder-bracket (arg)
  "Insert a balanced bracket or move past the closing one."
  (interactive "*P")
  (let ((lit (liter-p))
        (bpf blink-paren-function)
        blink-paren-function)
    (cond (lit (self-insert-command (prefix-numeric-value arg)))
          ((eq last-command-event ?\[)
           (insert ?\[)
           (plunder-balance ?\]))
          (t
           (plunder-move-past-close ?\])
           (when (and (not lit) (not executing-kbd-macro) bpf)
             (funcall bpf))))))

(defmacro cleanup-p (sym) `(memq ',sym c-cleanup-list))

(defmacro do-cleanup (&rest clauses)
  "do-cleanup is a dispatcher like cond that executes the forms in
a clause if some conditions are met.

Each clause has the form:
  (CLEANUP-TYPE [:delete-match N] RE*) FORMS.

If CLEANUP-TYPE is in `c-cleanup-list', and the list of regular
expressions RE* given as arguments to `re-bsearch' finds a match,
then the match number N (0 by default) is deleted and the
expressions FORMS are executed."
  `(cond
    ,@(mapcar #'(lambda (clause)
                  (destructuring-bind
                      ((type head &rest re) &rest body) clause
                    (let ((i 0))
                      (if (eq :delete-match head)
                          (progn
                            (setq i (car re))
                            (pop re))
                        (push head re))
                      `((and (cleanup-p ,type) (re-bsearch ,@re))
                        (delete-region (match-beginning ,i)
                                       (match-end ,i))
                        ,@(when (plusp i)
                            `((goto-char (match-beginning ,i))))
                        ,@body))))
              clauses)))

(defun brace-cleanup (stx)
  "Do various newline cleanups based on the settings of
`c-cleanup-list'. `stx' is the syntactic context of the line the
brace ends up on."
  (macrolet ((syntax-p (&rest args) `(c-intersect-lists ',args stx)))
    (case last-command-event
      (?\}
       (when (and (cleanup-p empty-defun-braces)
                  (syntax-p defun-close class-close inline-close)
                  (re-bsearch "{" "}\\="))
         (delete-region (1+ (match-beginning 0)) (1- (match-end 0))))
       (when (and (cleanup-p one-line-defun)
                  (syntax-p defun-close))
         (c-try-one-liner)))
      (?\{
       (do-cleanup ((brace-else-brace "}" "else" "{\\=")
                    (insert-and-inherit "} else {"))
                   ((brace-elseif-brace :delete-match 1
                                        "}" "else" "if" "(.*)\\(" "\\){\\=")
                    (insert ?\ )
                    (forward-char)))))))

(defun brace-cleanup-and-indent (lstx)
  "Indent and do some newline cleanups. The cursor is positioned
on the brace and `lstx' is the syntactic context of the original
line of the brace."
  (let* ((bstx (c-point-syntax))
         (newlines (c-brace-newlines bstx)))
    (macrolet ((newlines-p (sym) `(memq ',sym newlines)))
      (when (and (newlines-p before)
                 (> (current-column) (current-indentation)))
        (if c-syntactic-indentation
            (newline)                   ; indented only after cleanups
          (c-newline-and-indent)))
      (forward-char)
      (brace-cleanup (if (newlines-p before) bstx lstx))
      (when c-syntactic-indentation
        (c-indent-line))
      (when (newlines-p after)
        (c-newline-and-indent)))))

(defun plunder-electric-brace (arg)
  "This is a modified version of `c-electric-brace'.  It inserts
balanced braces or move past a closing one.

If `c-electric-flag' is non-nil, the brace is not inside a
literal and a numeric ARG hasn't been supplied, the command
performs several electric actions:

\(a) If the auto-newline feature is turned on (indicated by \"/la\" on
the mode line) newlines are inserted before and after the brace as
directed by the settings in `c-hanging-braces-alist'.

\(b) Any auto-newlines are indented.  The original line is also
reindented unless `c-syntactic-indentation' is nil.

\(c) If auto-newline is turned on, various newline cleanups based on the
settings of `c-cleanup-list' are done."
  (interactive "*P")
  (let ((bpf blink-paren-function) blink-paren-function sp lit)
    (c-save-buffer-state ()
      (setq sp (c-safe-position (point) (c-parse-state))
	    lit (c-in-literal sp)))

    (if (or lit (eq last-command-event ?\{))
        (self-insert-command (prefix-numeric-value arg))
      (plunder-move-past-close ?\}))

    (when (and c-electric-flag (not lit) (not arg))
      (cond ((looking-at "[ \t]*\\\\?$")
             (let (c-echo-syntactic-information-p ; shut this up
                   (lstx (c-save-buffer-state
                             ((c-syntactic-indentation-in-macros t)
                              (c-auto-newline-analysis t))
                           (c-guess-basic-syntax))))
               (when c-syntactic-indentation
                 (c-indent-line lstx))
               (when (and c-auto-newline (not (inlistp)))
                 (backward-char)
                 (brace-cleanup-and-indent lstx))))
            (c-syntactic-indentation (indent-according-to-mode))))

    ;; Blink the paren or balance with a closing brace.
    (unless lit
      (case last-command-event
        (?\{ (plunder-balance ?\}))
        (?\} (when (and (not executing-kbd-macro) bpf)
               (save-excursion
                 (c-save-buffer-state ()
                   (c-backward-syntactic-ws sp))
                 (funcall bpf))))))))

(defmacro delspaces (cond &rest body)
  "Syntax: delspaces (CLEANUP-TYPE :before RE :if COND) FORMS

If CLEANUP-TYPE is in `c-cleanup-list' and by researching
backward from point, we find whitespaces before RE, and COND is
true (the variables BEGIN-WS and END-WS are bound to the
beginning and end of the match) then the whitespaces are deleted
and FORMS are executed."
  (declare (indent defun))
  (destructuring-bind (type &key before if) cond
    `(save-excursion
       (when (and (cleanup-p ,type)
                  (re-search-backward
                   ,(concat "[^ \t]\\(.*\\)" before "\\=") nil t))
         (let ((begin-ws (match-beginning 1))
               (end-ws (match-end 1)))
           (when (c-save-buffer-state () ,if)
             (delete-region begin-ws end-ws)
             ,@(when body
                 `((goto-char begin-ws)
                   ,@body))))))))

(defun plunder-electric-paren (arg)
  "This is a modified version of `c-electric-paren'. It inserts
 balanced parenthesis or move past the closing one.

If `c-syntactic-indentation' and `c-electric-flag' are both
non-nil, the line is reindented unless a numeric ARG is supplied,
or the parenthesis is inserted inside a literal.

Whitespace between a function name and the parenthesis may get added or
removed; see the variable `c-cleanup-list'.

Also, if `c-electric-flag' and `c-auto-newline' are both non-nil, some
newline cleanups are done if appropriate; see the variable `c-cleanup-list'."
  (interactive "*P")
  (let ((lit (liter-p))
        c-echo-syntactic-information-p  ; shut this up
        (bpf blink-paren-function)
        blink-paren-function)
    (if (or lit (eq last-command-event ?\())
        (self-insert-command (prefix-numeric-value arg))
      (plunder-move-past-close ?\)))

    (when (and (not arg) (not lit))
      (when c-electric-flag
        (when c-syntactic-indentation
          (indent-according-to-mode))
        (when (and c-auto-newline
                   (eq last-command-event ?\()
                   (looking-at "[ \t]*\\\\?$"))
          (do-cleanup ((brace-elseif-brace "}" "else" "if" "(\\=")
                       (insert-and-inherit "} else if ("))
                      ((brace-catch-brace "}" "catch" "(\\=")
                       (insert-and-inherit "} catch (")))))

      (case last-command-event
        (?\( (delspaces (space-before-funcall
                         :before "("
                         :if (and (c-on-identifier)
                                  (not (and (c-beginning-of-macro)
                                            (c-forward-over-cpp-define-id)
                                            (eq (point) begin-ws)))))
               (insert ?\ ))
             (plunder-balance ?\)))
        (?\) (delspaces (compact-empty-funcall
                         :before "()"
                         :if (c-on-identifier)))
             (when (and (not executing-kbd-macro) bpf)
               (funcall bpf)))))))

(defsubst insert-blank ()
  (when (and (looking-at "[^ \t\n]")
             (looking-back "[^ \t\n]"))
    (insert ?\ )))

(defsubst sexp-endp ()
  (save-excursion (forward-sexp) (point)))

(defun sexp-or-region ()
  "Return the S-expression after point or the active region, the
locations of the beginning and the end the expression."
  (let (beg end)
    (if (and mark-active transient-mark-mode)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point)
            end (sexp-endp)))
    (values (buffer-substring beg end) beg end)))

(defun plunder-raise-sexp ()
  "Raise the S-expression after point or the active region to one
level of parenthesis, braces or brackets -- deleting the other
ones.

a*(b+|c-d) -> a* |c"

  (interactive "*")
  (multiple-value-bind (sexp beg end) (sexp-or-region)
    (let ((p (progn (plunder-backward-up-list) (point))))
      (delete-region p (sexp-endp))
      (insert sexp)
      (goto-char p)
      (insert-blank))))

(defun plunder-splice-sexp ()
  "Splice the list the point is on by removing its delimiters:
parenthesis, brackets or braces.

a * (b + |c) * d -> a * b + |c * d"

  (interactive "*")
  (save-excursion
    (plunder-backward-up-list)
    (let ((p (point)))
      (forward-sexp)
      (delete-char -1)
      (goto-char p)
      (delete-char 1)
      (insert-blank)
      (delete-trailing-whitespace)
      (condition-case nil
          (progn
            (plunder-backward-up-list)
            (c-indent-exp))
        ((Scan error) nil)))))

(defun plunder-wrap-sexp (open close)
  (multiple-value-bind (sexp beg end) (sexp-or-region)
    (delete-region beg end)
    (insert open)
    (insert close)
    (backward-char)
    (insert sexp)
    (plunder-backward-up-list)))

(defun plunder-wrap-sexp-in-paren ()
  "Wrap the S-expression at point or the active region inside
parenthesis.

|a + b -> |(a) + b"
  (interactive "*")
  (plunder-wrap-sexp ?\( ?\)))

(defun plunder-wrap-sexp-in-bracket ()
  "Wrap the S-expression at point or the active region inside
brackets.

a |i -> a |[i]"
  (interactive "*")
  (plunder-wrap-sexp ?\[ ?\]))

(defun plunder-wrap-sexp-in-brace ()
  "Wrap the statement after point or the active region inside
braces and indent the expression.

|statement -> |{ statement }"
  (interactive "*")
  (let (beg end)
    (if (and mark-active transient-mark-mode)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point)
            end (progn (c-end-of-statement) (point))))
    (let ((sexp (buffer-substring (progn (goto-char beg)
                                         (skip-chars-forward " \t\n")
                                         (point))
                                  end))
          (last-command-event ?\{))
      (delete-region beg end)
      (plunder-electric-brace nil)
      (insert sexp)
      (plunder-backward-up-list)
      (c-indent-exp))))

(provide 'plunder)
