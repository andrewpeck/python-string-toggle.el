;;; python-string-toggle.el --- Toggle between implicit concat and triple-quoted -*- lexical-binding: t; -*-

;; Author: Andrew Peck
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, python, convenience
;; URL: https://github.com/andrewpeck/python-string-toggle

;;; Commentary:

;; Provides `python-string-toggle' which converts the Python string at
;; point between two forms:
;;
;;   Implicit concatenation:
;;     text = ("first line\n"
;;             "second line\n"
;;             "third line")
;;
;;   Triple-quoted string:
;;     text = """\
;;     first line
;;     second line
;;     third line"""
;;
;; Place point inside the string and call `python-string-toggle'.
;;
;; Usage:
;;   (require 'python-string-toggle)
;;   (define-key python-mode-map (kbd "C-c C-e") #'python-string-toggle)

;;; Code:

(require 'python)

(defgroup python-string-toggle nil
  "Toggle Python strings between implicit concatenation and triple-quoted."
  :group 'python
  :prefix "python-string-toggle-")

;; ---------------------------------------------------------------------------
;; Low-level helpers
;; ---------------------------------------------------------------------------

(defun python-string-toggle--ppss (&optional pos)
  "Return `syntax-ppss' at POS (default: point)."
  (save-excursion
    (syntax-propertize (or pos (point)))
    (syntax-ppss (or pos (point)))))

(defun python-string-toggle--in-string-p (&optional pos)
  "Non-nil if POS (default: point) is inside a string."
  (nth 3 (python-string-toggle--ppss pos)))

(defun python-string-toggle--string-delim-start (&optional pos)
  "Return the delimiter start position reported by `syntax-ppss' at POS.
For triple-quoted strings this is the third quote character (the
generic-string-delimiter); for single-quoted strings it is the
opening quote."
  (nth 8 (python-string-toggle--ppss pos)))

(defun python-string-toggle--triple-quoted-p (delim-start)
  "Non-nil if DELIM-START has generic-string-delimiter syntax (class 15).
In Python mode this indicates a triple-quoted string."
  (when delim-start
    (let ((syn (syntax-after delim-start)))
      (and syn (eq (syntax-class syn) 15)))))

(defun python-string-toggle--quote-char (delim-start)
  "Return the quote character at DELIM-START."
  (char-after delim-start))

(defun python-string-toggle--string-prefix-at (pos)
  "Return any string prefix (f, r, b, u, etc.) ending just before POS."
  (save-excursion
    (goto-char pos)
    (let ((start pos))
      (while (and (> start (point-min))
                  (memq (char-before start) '(?f ?F ?b ?B ?r ?R ?u ?U)))
        (cl-decf start))
      (buffer-substring-no-properties start pos))))

(defun python-string-toggle--string-bounds (delim-start)
  "Return a plist-like list describing the string at DELIM-START.
The list is:
  (REAL-START CONTENT-START CONTENT-END REAL-END PREFIX QUOTE-CHAR TRIPLE-P)
where:
  REAL-START    — before any prefix and all opening quotes
  CONTENT-START — first character of the string body
  CONTENT-END   — one past last character of the string body
  REAL-END      — one past the final closing quote
  PREFIX        — string prefix like \"f\", \"rb\", etc.
  QUOTE-CHAR    — ?\\\" or ?\\='
  TRIPLE-P      — non-nil for triple-quoted strings"
  (let* ((triple (python-string-toggle--triple-quoted-p delim-start))
         (qchar (python-string-toggle--quote-char delim-start))
         content-start content-end real-start real-end prefix)
    (if triple
        ;; Triple-quoted: delim-start is the 3rd quote (generic-string-delimiter).
        ;; The two preceding characters are the first two quotes.
        (let* ((first-quote (- delim-start 2))
               (pfx (python-string-toggle--string-prefix-at first-quote)))
          (setq prefix pfx
                real-start (- first-quote (length pfx))
                content-start (1+ delim-start))
          (save-excursion
            (goto-char delim-start)
            (forward-sexp 1)
            ;; Point is now one past the closing generic-string-delimiter.
            ;; Two more plain-syntax quote chars follow it.
            (setq content-end (1- (point))
                  real-end (+ (point) 2))))
      ;; Single-quoted: delim-start is the opening quote itself.
      (let ((pfx (python-string-toggle--string-prefix-at delim-start)))
        (setq prefix pfx
              real-start (- delim-start (length pfx))
              content-start (1+ delim-start))
        (save-excursion
          (goto-char delim-start)
          (forward-sexp 1)
          (setq content-end (1- (point))
                real-end (point)))))
    (list real-start content-start content-end real-end prefix qchar triple)))

;; ---------------------------------------------------------------------------
;; Fragment collection for implicit concatenation
;; ---------------------------------------------------------------------------

(defun python-string-toggle--skip-ws-forward ()
  "Skip whitespace and comments forward."
  (forward-comment most-positive-fixnum))

(defun python-string-toggle--skip-ws-backward ()
  "Skip whitespace and comments backward."
  (forward-comment most-negative-fixnum))

(defun python-string-toggle--at-string-literal-p ()
  "If point is at the start of a string literal return its bounds; else nil."
  (save-excursion
    (when (looking-at "[fFbBrRuU]*[\"']")
      (goto-char (match-end 0))
      (backward-char 1)
      (syntax-propertize (min (+ (point) 4) (point-max)))
      (let* ((qc (char-after))
             (maybe-triple (and (< (+ (point) 2) (point-max))
                                (eq (char-after (1+ (point))) qc)
                                (eq (char-after (+ (point) 2)) qc)
                                (python-string-toggle--triple-quoted-p (+ (point) 2)))))
        (if maybe-triple
            (python-string-toggle--string-bounds (+ (point) 2))
          (python-string-toggle--string-bounds (point)))))))

(defun python-string-toggle--collect-fragments ()
  "Collect all implicitly concatenated string fragments around point.

Returns a list of bounds (from `python-string-toggle--string-bounds'),
sorted by position."
  (when (python-string-toggle--in-string-p)
    (let* ((ds (python-string-toggle--string-delim-start))
           (cur-bounds (python-string-toggle--string-bounds ds))
           (fragments (list cur-bounds)))
      ;; Scan backward for preceding fragments
      (save-excursion
        (goto-char (nth 0 cur-bounds))
        (let ((keep t))
          (while keep
            (python-string-toggle--skip-ws-backward)
            (let ((here (point)))
              (if (and (> here (point-min))
                       (python-string-toggle--in-string-p (1- here)))
                  (let* ((prev-ds (python-string-toggle--string-delim-start (1- here)))
                         (prev-bounds (when prev-ds (python-string-toggle--string-bounds prev-ds))))
                    (if (and prev-bounds (= (nth 3 prev-bounds) here))
                        (progn
                          (push prev-bounds fragments)
                          (goto-char (nth 0 prev-bounds)))
                      (setq keep nil)))
                (setq keep nil))))))
      ;; Scan forward for following fragments
      (save-excursion
        (goto-char (nth 3 cur-bounds))
        (let ((keep t))
          (while keep
            (python-string-toggle--skip-ws-forward)
            (let ((next (python-string-toggle--at-string-literal-p)))
              (if next
                  (progn
                    (setq fragments (nconc fragments (list next)))
                    (goto-char (nth 3 next)))
                (setq keep nil))))))
      (sort fragments (lambda (a b) (< (car a) (car b)))))))

;; ---------------------------------------------------------------------------
;; String content processing
;; ---------------------------------------------------------------------------

(defun python-string-toggle--unescape-for-triple (content quote-char)
  "Convert escape sequences in CONTENT from single-quoted form to raw triple form.
Turns literal \\n into real newlines, unescapes QUOTE-CHAR, etc."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (search-forward "\\n" nil t) (replace-match "\n" t t))
    (goto-char (point-min))
    (while (search-forward "\\t" nil t) (replace-match "\t" t t))
    (goto-char (point-min))
    (let ((pat (format "\\%c" quote-char)))
      (while (search-forward pat nil t)
        (replace-match (string quote-char) t t)))
    (buffer-string)))

(defun python-string-toggle--escape-for-single (line quote-char)
  "Escape LINE for use inside a single-quoted string delimited by QUOTE-CHAR."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (while (search-forward (string quote-char) nil t)
      (replace-match (format "\\%c" quote-char) t t))
    (buffer-string)))

(defun python-string-toggle--word-wrap (text max-width)
  "Split TEXT into a list of lines each at most MAX-WIDTH characters wide.
Breaks only at spaces.  Words longer than MAX-WIDTH are kept on their
own line rather than broken mid-word."
  (let ((words (split-string text " +" t))
        lines current)
    (dolist (word words)
      (let ((candidate (if current (concat current " " word) word)))
        (if (<= (length candidate) max-width)
            (setq current candidate)
          (when current (push current lines))
          (setq current word))))
    (when current (push current lines))
    (nreverse lines)))

;; ---------------------------------------------------------------------------
;; Conversion: implicit concat -> triple-quoted
;; ---------------------------------------------------------------------------

(defun python-string-toggle--implicit-to-triple (fragments)
  "Convert implicit-concatenation FRAGMENTS into a triple-quoted string."
  (let* ((first (car fragments))
         (last-frag (car (last fragments)))
         (real-start (nth 0 first))
         (real-end (nth 3 last-frag))
         (prefix (nth 4 first))
         (qchar (nth 5 first))
         (delim (make-string 3 qchar))
         ;; Collect and join content from every fragment
         (contents (mapcar (lambda (f)
                             (python-string-toggle--unescape-for-triple
                              (buffer-substring-no-properties (nth 1 f) (nth 2 f))
                              qchar))
                           fragments))
         (joined (string-join contents " "))
         ;; Detect surrounding parens
         (has-open-paren
          (save-excursion
            (goto-char real-start)
            (python-string-toggle--skip-ws-backward)
            (and (> (point) (point-min))
                 (eq (char-before) ?\())))
         (paren-start (when has-open-paren
                        (save-excursion
                          (goto-char real-start)
                          (python-string-toggle--skip-ws-backward)
                          (1- (point)))))
         (paren-end (when has-open-paren
                      (save-excursion
                        (goto-char real-end)
                        (python-string-toggle--skip-ws-forward)
                        (when (eq (char-after) ?\))
                          (1+ (point))))))
         (replace-start (or paren-start real-start))
         (replace-end (or paren-end real-end))
         (has-newlines (string-match-p "\n" joined))
         (base-col (save-excursion
                     (goto-char replace-start)
                     (current-column)))
         (content-indent (make-string (+ base-col 4) ?\s)))
    (let ((new-str
           (if has-newlines
               (let* ((lines (split-string joined "\n"))
                      (body (concat
                             (car lines) "\n"
                             (mapconcat (lambda (l) (concat content-indent l))
                                        (cdr lines) "\n"))))
                 (format "%s%s\\\n%s%s%s" prefix delim content-indent body delim))
             (format "%s%s%s%s" prefix delim joined delim))))
      (delete-region replace-start replace-end)
      (goto-char replace-start)
      (insert new-str))))

;; ---------------------------------------------------------------------------
;; Conversion: triple-quoted -> implicit concat
;; ---------------------------------------------------------------------------

(defun python-string-toggle--triple-to-implicit (bounds)
  "Convert a triple-quoted string (BOUNDS) to implicit concatenation.
Each line is stripped of leading whitespace and emitted as a separate
string fragment with no trailing \\n.  Blank lines in the original
produce a \"\\n\" fragment to preserve paragraph breaks."
  (let* ((real-start (nth 0 bounds))
         (content-start (nth 1 bounds))
         (content-end (nth 2 bounds))
         (real-end (nth 3 bounds))
         (prefix (nth 4 bounds))
         (qchar (nth 5 bounds))
         (qstr (string qchar))
         (raw (buffer-substring-no-properties content-start content-end))
         ;; Strip leading backslash-newline or bare newline
         (content (cond
                   ((string-prefix-p "\\\n" raw) (substring raw 2))
                   ((string-prefix-p "\n" raw) (substring raw 1))
                   (t raw)))
         ;; Strip trailing newline
         (content (if (string-suffix-p "\n" content)
                      (substring content 0 -1)
                    content))
         (lines (split-string content "\n"))
         ;; Drop leading and trailing blank lines
         (lines (progn
                  (while (and lines (string-blank-p (car lines)))
                    (setq lines (cdr lines)))
                  (while (and lines (string-blank-p (car (last lines))))
                    (setq lines (butlast lines)))
                  lines))
         ;; Use the line's own indentation level so fragments align under
         ;; the assignment, not under the opening quotes.
         (base-col (save-excursion
                     (goto-char real-start)
                     (current-indentation)))
         (indent-str (make-string (+ base-col 4) ?\s))
         (close-indent (make-string base-col ?\s))
         ;; If there is only one (possibly long) line, word-wrap it so the
         ;; output has reasonable line lengths.
         (lines (if (= (length lines) 1)
                    (python-string-toggle--word-wrap
                     (string-trim-left (car lines))
                     (- fill-column (+ base-col 4) (length prefix) 2))
                  lines))
         ;; blank lines → "\n" literal; non-blank → strip leading ws, quote
         (frag-strings
          (cl-loop for line in lines
                   for trimmed = (string-trim-left line)
                   if (string-blank-p trimmed)
                   collect (format "%s%s\\n%s" prefix qstr qstr)
                   else
                   collect (format "%s%s%s%s" prefix qstr
                                   (python-string-toggle--escape-for-single trimmed qchar)
                                   qstr))))
    (let ((new-str
           (if (<= (length frag-strings) 1)
               (or (car frag-strings) "")
             (concat "(\n"
                     (mapconcat (lambda (s) (concat indent-str s))
                                frag-strings "\n")
                     "\n" close-indent ")"))))
      (delete-region real-start real-end)
      (goto-char real-start)
      (insert new-str))))

;; ---------------------------------------------------------------------------
;; Main command
;; ---------------------------------------------------------------------------

;;;###autoload
(defun python-string-toggle ()
  "Toggle the Python string at point between implicit concat and triple-quoted.

When point is inside implicitly concatenated strings like:

    (\"first line\\n\"
     \"second line\\n\"
     \"third line\")

it converts them to a triple-quoted string:

    \"\"\"\
    first line
    second line
    third line\"\"\"

And vice versa.  String prefixes (f, r, b, etc.) are preserved."
  (interactive)
  (unless (derived-mode-p 'python-mode 'python-ts-mode)
    (user-error "This command is only for Python buffers"))
  (syntax-propertize (point-max))
  (unless (python-string-toggle--in-string-p)
    (user-error "Point is not inside a string"))
  (let ((fragments (python-string-toggle--collect-fragments)))
    (cond
     ;; Multiple non-triple fragments → triple-quoted
     ((and (> (length fragments) 1)
           (cl-every (lambda (f) (not (nth 6 f))) fragments))
      (save-excursion (python-string-toggle--implicit-to-triple fragments))
      (message "Converted to triple-quoted string"))
     ;; Single triple-quoted → implicit concatenation
     ((and (= (length fragments) 1)
           (nth 6 (car fragments)))
      (save-excursion (python-string-toggle--triple-to-implicit (car fragments)))
      (message "Converted to implicit concatenation"))
     ;; Single non-triple → wrap in triple quotes
     ((and (= (length fragments) 1)
           (not (nth 6 (car fragments))))
      (save-excursion (python-string-toggle--implicit-to-triple fragments))
      (message "Wrapped in triple quotes"))
     (t
      (user-error "Could not determine string style at point")))))

(provide 'python-string-toggle)
;;; python-string-toggle.el ends here
