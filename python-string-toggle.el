;;; python-string-toggle.el --- Toggle between implicit concat and triple-quoted -*- lexical-binding: t; -*-

;; Author: Andrew Peck
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, python, convenience
;; URL: https://github.com/andrewpeck/python-string-toggle

;;; Commentary:

;; Provides `python-string-toggle' for `python-ts-mode'.  It converts the
;; string expression at point between two forms:
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
;; The implementation uses tree-sitter's Python grammar to locate either a
;; `string' or `concatenated_string' node at point, then rebuilds the source
;; from the captured node spans.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'treesit)

(defgroup python-string-toggle nil
  "Toggle Python strings between implicit concatenation and triple-quoted."
  :group 'python
  :prefix "python-string-toggle-")

(defconst python-string-toggle--fragment-query
  '((string) @fragment)
  "Query used to collect string fragments from a `concatenated_string' node.")

(defconst python-string-toggle--part-query
  '(((string_content) @part)
    ((interpolation) @part))
  "Query used to collect content-bearing children from a `string' node.")

(defun python-string-toggle--ensure-context ()
  "Ensure the current buffer is ready for `python-string-toggle'."
  (unless (derived-mode-p 'python-ts-mode)
    (user-error "This command only supports python-ts-mode"))
  (unless (treesit-language-available-p 'python)
    (user-error "Python tree-sitter grammar is not available"))
  (unless (treesit-parser-list nil 'python)
    (treesit-parser-create 'python)))

(defun python-string-toggle--node-type-p (node type)
  "Return non-nil when NODE's type is TYPE."
  (and node (string= (treesit-node-type node) type)))

(defun python-string-toggle--string-expression-at-point ()
  "Return the `string' or `concatenated_string' node at point."
  (let* ((leaf (treesit-node-at (point) 'python))
         (concat-node
          (treesit-parent-until
           leaf
           (lambda (node)
             (python-string-toggle--node-type-p node "concatenated_string"))
           t))
         (string-node
          (treesit-parent-until
           leaf
           (lambda (node)
             (python-string-toggle--node-type-p node "string"))
           t)))
    (or concat-node string-node)))

(defun python-string-toggle--fragment-nodes (expression-node)
  "Return the string fragment nodes contained in EXPRESSION-NODE."
  (cond
   ((python-string-toggle--node-type-p expression-node "concatenated_string")
    (treesit-query-capture
     expression-node python-string-toggle--fragment-query nil nil t))
   ((python-string-toggle--node-type-p expression-node "string")
    (list expression-node))
   (t nil)))

(defun python-string-toggle--parse-string-start (text)
  "Parse string start token TEXT and return (PREFIX QUOTE-CHAR TRIPLE-P)."
  (unless (string-match "\\`\\([fFbBrRuU]*\\)\\(\"\"\"\\|'''\\|\"\\|'\\)\\'" text)
    (user-error "Unsupported Python string delimiter: %s" text))
  (let* ((prefix (match-string 1 text))
         (delimiter (match-string 2 text)))
    (list prefix
          (aref delimiter 0)
          (= (length delimiter) 3))))

(defun python-string-toggle--string-node-info (node)
  "Return parsed metadata for string NODE."
  (let* ((start-node (treesit-node-child node 0))
         (parts (mapcar #'cdr
                        (treesit-query-capture node python-string-toggle--part-query)))
         (start-token (treesit-node-text start-node t))
         (start-info (python-string-toggle--parse-string-start start-token))
         (prefix (nth 0 start-info))
         (qchar (nth 1 start-info))
         (triple-p (nth 2 start-info))
         (content (mapconcat (lambda (part) (treesit-node-text part t)) parts "")))
    (list :node node
          :real-start (treesit-node-start node)
          :real-end (treesit-node-end node)
          :prefix prefix
          :quote-char qchar
          :triple-p triple-p
          :content content)))

(defun python-string-toggle--fragment-infos (expression-node)
  "Return parsed fragment infos for EXPRESSION-NODE."
  (mapcar #'python-string-toggle--string-node-info
          (python-string-toggle--fragment-nodes expression-node)))

(defun python-string-toggle--compatible-fragments-p (fragments)
  "Return non-nil when FRAGMENTS share prefix and quote character."
  (let ((prefix (plist-get (car fragments) :prefix))
        (qchar (plist-get (car fragments) :quote-char)))
    (cl-every (lambda (fragment)
                (and (equal (plist-get fragment :prefix) prefix)
                     (= (plist-get fragment :quote-char) qchar)))
              fragments)))

(defun python-string-toggle--replacement-node (expression-node)
  "Return the node span that should be replaced for EXPRESSION-NODE."
  (let* ((start (treesit-node-start expression-node))
         (end (treesit-node-end expression-node))
         (open-pos
          (save-excursion
            (goto-char start)
            (skip-chars-backward " \t\r\n")
            (when (eq (char-before) ?\()
              (1- (point)))))
         (close-pos
          (save-excursion
            (goto-char end)
            (skip-chars-forward " \t\r\n")
            (when (eq (char-after) ?\))
              (point)))))
    (if (and open-pos close-pos)
        (list open-pos (1+ close-pos))
      (list start end))))

(defun python-string-toggle--unescape-for-triple (content quote-char)
  "Convert escape sequences in CONTENT from single-quoted form to triple form.
Turns literal \\n into real newlines and unescapes QUOTE-CHAR."
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
  "Split TEXT into a list of lines each at most MAX-WIDTH characters wide."
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

(defun python-string-toggle--joined-content (fragments)
  "Return the combined textual content of FRAGMENTS."
  (let ((contents (mapcar (lambda (fragment)
                            (python-string-toggle--unescape-for-triple
                             (plist-get fragment :content)
                             (plist-get fragment :quote-char)))
                          fragments)))
    (if contents
        (cl-reduce (lambda (acc content)
                     (concat acc
                             (if (string-suffix-p "\n" acc) "" " ")
                             content))
                   (cdr contents)
                   :initial-value (car contents))
      "")))

(defun python-string-toggle--implicit-to-triple (expression-node fragments)
  "Convert implicit-concatenation FRAGMENTS in EXPRESSION-NODE into triple quotes."
  (unless (python-string-toggle--compatible-fragments-p fragments)
    (user-error "String fragments use mixed prefixes or quote styles"))
  (let* ((replacement-node (python-string-toggle--replacement-node expression-node))
         (replace-start (car replacement-node))
         (replace-end (cadr replacement-node))
         (prefix (plist-get (car fragments) :prefix))
         (qchar (plist-get (car fragments) :quote-char))
         (delim (make-string 3 qchar))
         (joined (python-string-toggle--joined-content fragments))
         (needs-fill-p (> (length fragments) 1))
         (new-str (format "%s%s%s%s" prefix delim joined delim)))
    (delete-region replace-start replace-end)
    (goto-char replace-start)
    (insert new-str)
    (when needs-fill-p
      (save-excursion
        (goto-char (+ replace-start (length prefix) (length delim) 1))
        (fill-paragraph nil)))))

(defun python-string-toggle--triple-to-implicit (fragment)
  "Convert triple-quoted FRAGMENT into implicit concatenation."
  (let* ((real-start (plist-get fragment :real-start))
         (real-end (plist-get fragment :real-end))
         (prefix (plist-get fragment :prefix))
         (qchar (plist-get fragment :quote-char))
         (qstr (string qchar))
         (raw (plist-get fragment :content))
         (content (cond
                   ((string-prefix-p "\\\n" raw) (substring raw 2))
                   ((string-prefix-p "\n" raw) (substring raw 1))
                   (t raw)))
         (content (if (string-suffix-p "\n" content)
                      (substring content 0 -1)
                    content))
         (lines (split-string content "\n"))
         (lines (progn
                  (while (and lines (string-blank-p (car lines)))
                    (setq lines (cdr lines)))
                  (while (and lines (string-blank-p (car (last lines))))
                    (setq lines (butlast lines)))
                  lines))
         (base-col (save-excursion
                     (goto-char real-start)
                     (current-indentation)))
         (indent-str (make-string (+ base-col 4) ?\s))
         (close-indent (make-string base-col ?\s))
         (lines (if (= (length lines) 1)
                    (python-string-toggle--word-wrap
                     (string-trim-left (car lines))
                     (- fill-column (+ base-col 4) (length prefix) 2))
                  lines))
         (frag-strings
          (cl-loop for line in lines
                   for trimmed = (string-trim-left line)
                   if (string-blank-p trimmed)
                   collect (format "%s%s\\n%s" prefix qstr qstr)
                   else
                   collect (format "%s%s%s%s"
                                   prefix
                                   qstr
                                   (python-string-toggle--escape-for-single trimmed qchar)
                                   qstr)))
         (new-str
          (if (<= (length frag-strings) 1)
              (or (car frag-strings) "")
            (concat "(\n"
                    (mapconcat (lambda (fragment-string)
                                 (concat indent-str fragment-string))
                               frag-strings "\n")
                    "\n" close-indent ")"))))
    (delete-region real-start real-end)
    (goto-char real-start)
    (insert new-str)))

;;;###autoload
(defun python-string-toggle ()
  "Toggle the Python string at point between implicit concat and triple-quoted.

This command only supports `python-ts-mode'."
  (interactive)
  (python-string-toggle--ensure-context)
  (let* ((expression-node (python-string-toggle--string-expression-at-point))
         (fragments (and expression-node
                         (python-string-toggle--fragment-infos expression-node))))
    (unless fragments
      (user-error "Point is not inside a Python string"))
    (cond
     ((and (> (length fragments) 1)
           (cl-every (lambda (fragment)
                       (not (plist-get fragment :triple-p)))
                     fragments))
      (save-excursion
        (python-string-toggle--implicit-to-triple expression-node fragments))
      (message "Converted to triple-quoted string"))
     ((and (= (length fragments) 1)
           (plist-get (car fragments) :triple-p))
      (save-excursion
        (python-string-toggle--triple-to-implicit (car fragments)))
      (message "Converted to implicit concatenation"))
     ((and (= (length fragments) 1)
           (not (plist-get (car fragments) :triple-p)))
      (save-excursion
        (python-string-toggle--implicit-to-triple expression-node fragments))
      (message "Wrapped in triple quotes"))
     (t
      (user-error "Could not determine string style at point")))))

(provide 'python-string-toggle)
;;; python-string-toggle.el ends here
;; LocalWords: infos unescapes
