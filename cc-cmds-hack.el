(eval-when-compile
  (require 'cc-cmds)
  (require 'cl))

(defun inlistp ()
  "Return true if we're inside a C list."
  (c-intersect-lists
   '(brace-list-intro brace-list-entry brace-entry-open)
   (c-save-buffer-state () (c-guess-basic-syntax))))

(defsubst newlinep (close)
  (and (eq close ?\}) (not (inlistp))))

(defun c-hack-balance (close)
  "Insert a corresponding closing token and eventually add a
newline."
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

(defun c-hack-backward-up-list (&optional forward-p)
  "Move backward or forward if forward-p is true out of one level
of parentheses."
  (interactive)
  (flet ((matchp (l r)
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
  (let ((space "\\(?:[ \t\n]\\|\\\\\n\\)*"))
    `(save-excursion
       (and (re-search-backward
             (concat ,@(mapcon #'(lambda (args)
                                   (if (cdr args)
                                       (list (car args) space)
                                     (list (car args))))
                               res))
             nil t)
            (not (liter-p))))))

(defun c-hack-move-past-close (close)
  "Delete the trailing blanks before the closing token and move
past it, eventually leaving a newline.  It's possible to move
past the closing token inside a nested expression."
  (interactive "*")
  (loop with rtok
        do
        (condition-case ()
            (c-hack-backward-up-list t)
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

(defun c-hack-bracket (arg)
  "Insert a balanced bracket or move past the closing one."
  (interactive "*P")
  (let ((lit (liter-p))
        (bpf blink-paren-function)
        blink-paren-function)
    (cond (lit (self-insert-command (prefix-numeric-value arg)))
          ((eq last-command-event ?\[)
           (insert ?\[)
           (c-hack-balance ?\]))
          (t
           (c-hack-move-past-close ?\])
           (when (and (not lit) (not executing-kbd-macro) bpf)
             (funcall bpf))))))

(defmacro cleanup-p (sym) `(memq ',sym c-cleanup-list))

(defmacro do-cleanup (&rest entries)
  `(cond
    ,@(mapcar #'(lambda (entry)
                  (destructuring-bind
                      ((type head &rest re) &rest body) entry
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
              entries)))

(defun brace-cleanup (syn)
  "Do various newline cleanups based on the settings of
`c-cleanup-list'. `syn' is the syntactic context of the line the
brace ends up on."
  (macrolet ((syntax-p (&rest args) `(c-intersect-lists ',args syn)))
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

(defun brace-cleanup-and-indent (lsyn)
  "Indent and do some newline cleanups. The cursor is positioned
on the brace and `lsyn' is the syntactic context of the original
line of the brace."
  (let* ((bsyn (c-point-syntax))
         (newlines (c-brace-newlines bsyn)))
    (macrolet ((newlines-p (sym) `(memq ',sym newlines)))
      (when (and (newlines-p before)
                 (> (current-column) (current-indentation)))
        (if c-syntactic-indentation
            (newline)                   ; indented only after cleanups
          (c-newline-and-indent)))
      (forward-char)
      (brace-cleanup (if (newlines-p before) bsyn lsyn))
      (when c-syntactic-indentation
        (c-indent-line))
      (when (newlines-p after)
        (c-newline-and-indent)))))

(defun c-hack-electric-brace (arg)
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
      (c-hack-move-past-close ?\}))

    (when (and c-electric-flag (not lit) (not arg))
      (cond ((looking-at "[ \t]*\\\\?$")
             (let (c-echo-syntactic-information-p ; shut this up
                   (lsyn (c-save-buffer-state
                             ((c-syntactic-indentation-in-macros t)
                              (c-auto-newline-analysis t))
                           (c-guess-basic-syntax))))
               (when c-syntactic-indentation
                 (c-indent-line lsyn))
               (when (and c-auto-newline (not (inlistp)))
                 (backward-char)
                 (brace-cleanup-and-indent lsyn))))
            (c-syntactic-indentation (indent-according-to-mode))))

    ;; Blink the paren or balance with a closing brace.
    (unless lit
      (case last-command-event
        (?\{ (c-hack-balance ?\}))
        (?\} (when (and (not executing-kbd-macro) bpf)
               (save-excursion
                 (c-save-buffer-state ()
                   (c-backward-syntactic-ws sp))
                 (funcall bpf))))))))

(defun c-hack-electric-paren (arg)
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
      (c-hack-move-past-close ?\)))

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

      ;; Check for clean-ups at function calls.  These two DON'T need
      ;; `c-electric-flag' or `c-syntactic-indentation' set.
      ;; Point is currently just after the inserted paren.
      (let (beg (end (1- (point))))
        (case last-command-event
          (?\( (save-excursion ; don't add a space into #define Foo()...
                 (when (and (cleanup-p space-before-funcall)
                            (re-search-backward "[^ \t]\\(.*\\)(\\=" nil t)
                            (save-match-data
                              (c-save-buffer-state ((p (1+ (point))))
                                (and (c-on-identifier)
                                     (not (and (c-beginning-of-macro)
                                               (c-forward-over-cpp-define-id)
                                               (eq p (point))))))))
                   (delete-region (match-beginning 1) (match-end 1))
                   (goto-char (match-beginning 1))
                   (insert ?\ )))
               (c-hack-balance ?\)))
          (?\) (save-excursion
                 (when (and (cleanup-p compact-empty-funcall)
                            (re-search-backward "[^ \t]\\(.*\\)()\\=" nil t)
                            (save-match-data
                              (c-save-buffer-state () (c-on-identifier))))
                   (delete-region (match-beginning 1) (match-end 1))))
               (when (and (not executing-kbd-macro) bpf)
                 (funcall bpf))))))))

(defun c-hack-snug-do-while (syntax pos)
  "This function is a modified version of `c-snug-do-while' that
works with macros."
  (save-excursion
    (if (and (eq syntax 'block-close)
             (progn (backward-up-list)
                    (c-forward-sexp -1)
                    (looking-at "\\<do\\>[^_]")))
        '(before)
      '(before after))))

(defsubst insert-blank (c)
  (unless (or (char-equal c ?\t)
              (char-equal c ?\ )
              (char-equal c ?\n))
    (insert ?\ )))

(defun c-hack-raise-sexp ()
  (interactive "*")
  (flet ((sexp-endp () (save-excursion (forward-sexp) (point))))
    (let ((sexp (buffer-substring (point) (sexp-endp))))
      (c-hack-backward-up-list)
      (delete-region (point) (sexp-endp))
      (insert-blank (char-before))
      (insert sexp))))

(defun c-hack-splice-sexp ()
  (interactive "*")
  (save-excursion
    (c-hack-backward-up-list)
    (let ((p (point)) (c (char-before)))
      (forward-sexp)
      (delete-backward-char 1)
      (goto-char p)
      (delete-char 1)
      (insert-blank c))))

(provide 'cc-cmds-hack)
