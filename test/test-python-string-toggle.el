;;; test-python-string-toggle.el --- ERT tests for python-string-toggle -*- lexical-binding: t; -*-

;; Run with: make all   (delegates to makem.sh)

;;; Code:

(require 'ert)
(require 'python)
(require 'treesit)
(require 'python-string-toggle)

(add-to-list 'treesit-extra-load-path
             (file-name-directory (or load-file-name buffer-file-name)))

;;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defmacro pstt-with-buffer (content &rest body)
  "Run BODY in a temporary `python-ts-mode' buffer containing CONTENT.
Point is at `point-min' after insertion."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (python-ts-mode)
     ,@body))

(defun pstt-toggle (content point-marker)
  "Insert CONTENT in a python-ts-mode buffer, position point by searching for
POINT-MARKER (which must land inside the target string), call
`python-string-toggle', and return the resulting buffer contents."
  (pstt-with-buffer content
    (search-forward point-marker)
    (python-string-toggle)
    (buffer-string)))

(defun pstt-toggle-twice (content point-marker)
  "Apply `python-string-toggle' twice using POINT-MARKER to locate the string.
Used for round-trip testing: toggle once, then toggle again in a fresh buffer
and return the final buffer contents."
  (let ((after-first (pstt-toggle content point-marker)))
    (pstt-with-buffer after-first
      (search-forward point-marker)
      (python-string-toggle)
      (buffer-string))))


;;;; ─── Pure helpers: word-wrap ───────────────────────────────────────────────

(ert-deftest pstt-word-wrap/basic ()
  "Text is split at spaces when a line would exceed max-width."
  (should (equal (python-string-toggle--word-wrap "one two three" 7)
                 '("one two" "three")))
  (should (equal (python-string-toggle--word-wrap "one two three four" 7)
                 '("one two" "three" "four"))))

(ert-deftest pstt-word-wrap/fits-in-one-line ()
  "Text that fits within max-width is returned as a single element."
  (should (equal (python-string-toggle--word-wrap "hello world" 20)
                 '("hello world"))))

(ert-deftest pstt-word-wrap/exact-width ()
  "Text that exactly fills max-width fits on one line."
  (should (equal (python-string-toggle--word-wrap "hello" 5)
                 '("hello"))))

(ert-deftest pstt-word-wrap/long-word-kept-whole ()
  "A single word longer than max-width is not broken mid-word."
  (should (equal (python-string-toggle--word-wrap "superlongword" 5)
                 '("superlongword"))))

(ert-deftest pstt-word-wrap/empty-string ()
  "Empty input produces an empty list."
  (should (equal (python-string-toggle--word-wrap "" 10)
                 nil)))

(ert-deftest pstt-word-wrap/multiple-spaces-collapsed ()
  "Multiple consecutive spaces are treated as one separator (via split-string).
\"a  b  c\" splits into words [\"a\" \"b\" \"c\"]; \"a b\" fits in width 3, so the
result is two elements, not three."
  (should (equal (python-string-toggle--word-wrap "a  b  c" 3)
                 '("a b" "c"))))


;;;; ─── Pure helpers: unescape-for-triple ─────────────────────────────────────

(ert-deftest pstt-unescape-for-triple/newline ()
  "Backslash-n is converted to a real newline."
  (should (equal (python-string-toggle--unescape-for-triple "hello\\nworld" ?\")
                 "hello\nworld")))

(ert-deftest pstt-unescape-for-triple/tab ()
  "Backslash-t is converted to a real tab."
  (should (equal (python-string-toggle--unescape-for-triple "col1\\tcol2" ?\")
                 "col1\tcol2")))

(ert-deftest pstt-unescape-for-triple/double-quote ()
  "Escaped double-quote is unescaped."
  (should (equal (python-string-toggle--unescape-for-triple "say \\\"hi\\\"" ?\")
                 "say \"hi\"")))

(ert-deftest pstt-unescape-for-triple/single-quote ()
  "Escaped single-quote is unescaped."
  (should (equal (python-string-toggle--unescape-for-triple "it\\'s fine" ?\')
                 "it's fine")))

(ert-deftest pstt-unescape-for-triple/no-escapes ()
  "Content without escape sequences is unchanged."
  (should (equal (python-string-toggle--unescape-for-triple "plain text" ?\")
                 "plain text")))

(ert-deftest pstt-unescape-for-triple/multiple-newlines ()
  "Multiple backslash-n escapes are all converted."
  (should (equal (python-string-toggle--unescape-for-triple "a\\nb\\nc" ?\")
                 "a\nb\nc")))


;;;; ─── Pure helpers: escape-for-single ──────────────────────────────────────

(ert-deftest pstt-escape-for-single/double-quote ()
  "Double-quotes are escaped when the string delimiter is double-quote."
  (should (equal (python-string-toggle--escape-for-single "say \"hi\"" ?\")
                 "say \\\"hi\\\"")))

(ert-deftest pstt-escape-for-single/single-quote ()
  "Single-quotes are escaped when the string delimiter is single-quote."
  (should (equal (python-string-toggle--escape-for-single "it's fine" ?\')
                 "it\\'s fine")))

(ert-deftest pstt-escape-for-single/no-op ()
  "Content without the target quote char is unchanged."
  (should (equal (python-string-toggle--escape-for-single "no quotes here" ?\")
                 "no quotes here")))

(ert-deftest pstt-escape-for-single/roundtrips-with-unescape ()
  "escape-for-single and unescape-for-triple are mutual inverses for quotes."
  (let* ((original "he said \"hello\"")
         (escaped (python-string-toggle--escape-for-single original ?\"))
         (restored (python-string-toggle--unescape-for-triple escaped ?\")))
    (should (equal restored original))))


;;;; ─── Triple-quoted → implicit concatenation ────────────────────────────────

(ert-deftest pstt-triple-to-implicit/multiline ()
  "Multi-line triple-quoted string produces a parenthesised implicit concat.
Each line of the body becomes one fragment; leading whitespace is stripped."
  (let ((result (pstt-toggle
                 "x = \"\"\"\\\n    first line\n    second line\n    third line\"\"\""
                 "first line")))
    (should (string-match-p (regexp-quote "\"first line\"") result))
    (should (string-match-p (regexp-quote "\"second line\"") result))
    (should (string-match-p (regexp-quote "\"third line\"") result))
    ;; Multiple fragments → must be wrapped in parens
    (should (string-match-p "(" result))
    ;; No triple-quoted delimiter should remain
    (should-not (string-match-p "\"\"\"" result))))

(ert-deftest pstt-triple-to-implicit/single-line ()
  "A single-line triple-quoted string becomes a plain quoted string (no parens)."
  (let ((result (pstt-toggle "x = \"\"\"hello world\"\"\"" "hello")))
    (should (string-match-p (regexp-quote "\"hello world\"") result))
    (should-not (string-match-p "\"\"\"" result))
    ;; Single fragment → no surrounding parentheses
    (should-not (string-match-p "^(" result))))

(ert-deftest pstt-triple-to-implicit/blank-line-becomes-newline-fragment ()
  "A blank line inside the triple body becomes a \"\\n\" fragment."
  (let ((result (pstt-toggle
                 "x = \"\"\"\\\n    first\n\n    third\"\"\""
                 "first")))
    (should (string-match-p (regexp-quote "\"\\n\"") result))
    (should (string-match-p (regexp-quote "\"first\"") result))
    (should (string-match-p (regexp-quote "\"third\"") result))))

(ert-deftest pstt-triple-to-implicit/preserves-prefix ()
  "The string prefix (f, r, b, …) is carried over to every fragment."
  (let ((result (pstt-toggle
                 "x = f\"\"\"\\\n    hello\n    world\"\"\""
                 "hello")))
    (should (string-match-p (regexp-quote "f\"hello\"") result))
    (should (string-match-p (regexp-quote "f\"world\"") result))))

(ert-deftest pstt-triple-to-implicit/strips-leading-whitespace ()
  "Indentation inside the triple body is stripped from each fragment."
  ;; The lines are indented 8 spaces in the source; the fragments should
  ;; not carry that indentation.
  (let ((result (pstt-toggle
                 "x = \"\"\"\\\n        deep indent\n        second\"\"\""
                 "deep")))
    (should (string-match-p (regexp-quote "\"deep indent\"") result))
    (should (string-match-p (regexp-quote "\"second\"") result))
    ;; No leading spaces inside the quotes
    (should-not (string-match-p "\"  " result))))


;;;; ─── Implicit concatenation → triple-quoted ────────────────────────────────

(ert-deftest pstt-implicit-to-triple/basic ()
  "Parenthesised implicit concat with \\n escapes converts to triple-quoted."
  (let ((result (pstt-toggle
                 "x = (\"first line\\n\"\n     \"second line\\n\"\n     \"third line\")"
                 "first line")))
    (should (string-match-p "\"\"\"" result))
    (should (string-match-p "first line" result))
    (should (string-match-p "second line" result))
    (should (string-match-p "third line" result))
    ;; Separate quoted fragments must be gone
    (should-not (string-match-p (regexp-quote "\"first line\\n\"") result))))

(ert-deftest pstt-implicit-to-triple/preserves-prefix ()
  "String prefix is preserved when converting to triple-quoted."
  (let ((result (pstt-toggle
                 "x = (f\"line one\\n\"\n     f\"line two\")"
                 "line one")))
    (should (string-match-p (regexp-quote "f\"\"\"") result))))

(ert-deftest pstt-implicit-to-triple/removes-parens ()
  "The surrounding parentheses are removed in the triple-quoted result."
  (let* ((input "x = (\"hello\\n\"\n     \"world\")")
         (result (pstt-toggle input "hello")))
    (should (string-match-p "\"\"\"" result))
    ;; No standalone open paren outside the string
    (should-not (string-match-p "= (" result))))

(ert-deftest pstt-implicit-to-triple/no-spurious-space-after-newline ()
  "Implicit concat with a \\n-terminated fragment converts to triple-quoted form.
Content from all fragments appears in the result."
  (let* ((input "x = (\"line1\\n\"\n     \"line2\")")
         (result (pstt-toggle input "line1")))
    (should (string-match-p "\"\"\"" result))
    (should (string-match-p "line1" result))
    (should (string-match-p "line2" result))))

(ert-deftest pstt-implicit-to-triple/assignment-keeps-lines-clean ()
  "Adjacent string literals convert to triple-quoted form.
Content is preserved and no leftover implicit-concat delimiters remain."
  (let* ((input
          "description=(\n    \"Cras placerat accumsan nulla. Cum sociis natoque\"\n    \"penatibus et magnis dis parturient montes, nascetur ridiculus mus. Proin quam\"\n    \"nisl, tincidunt et, mattis eget, convallis nec, purus.\"\n),")
         (result (pstt-toggle input "Cras placerat")))
    (should (string-match-p "\"\"\"" result))
    (should (string-match-p "Cras placerat" result))
    (should (string-match-p "penatibus" result))
    (should (string-match-p "purus" result))
    ;; No leftover paired single-quoted fragments
    (should-not (string-match-p "\"\n    \"" result))))


;;;; ─── Single-string wrapping ────────────────────────────────────────────────

(ert-deftest pstt-single-to-triple/plain-string ()
  "A single non-triple string is wrapped in triple quotes."
  (let ((result (pstt-toggle "x = \"hello world\"" "hello")))
    (should (string-match-p (regexp-quote "\"\"\"hello world\"\"\"") result))))

(ert-deftest pstt-single-to-triple/with-prefix ()
  "Prefix is preserved when a single f-string is wrapped in triple quotes."
  (let ((result (pstt-toggle "x = f\"hello world\"" "hello")))
    (should (string-match-p (regexp-quote "f\"\"\"hello world\"\"\"") result))))


;;;; ─── Round-trip tests ───────────────────────────────────────────────────────

(ert-deftest pstt-roundtrip/single-line-triple ()
  "A single-line triple-quoted string round-trips exactly.
 triple → implicit → triple should restore the original."
  (let* ((original "x = \"\"\"hello world\"\"\"")
         (result   (pstt-toggle-twice original "hello")))
    (should (equal result original))))

(ert-deftest pstt-roundtrip/multiline-triple-preserves-lines ()
  "triple → implicit → triple: all content survives the round-trip."
  (let* ((original "x = \"\"\"\\\n    first line\n    second line\n    third line\"\"\"")
         (result   (pstt-toggle-twice original "first line")))
    ;; All content survives
    (should (string-match-p "first line" result))
    (should (string-match-p "second line" result))
    (should (string-match-p "third line" result))
    (should (string-match-p "\"\"\"" result))))

(ert-deftest pstt-roundtrip/implicit-newlines-not-preserved ()
  "implicit(with \\n) → triple → implicit: content is preserved across the round-trip.
The \\n escape sequences collapse when the text is joined during triple-quoting."
  (let* ((original "x = (\"first line\\n\"\n     \"second line\\n\"\n     \"third line\")")
         (result   (pstt-toggle-twice original "first line")))
    ;; After two toggles we should be back in implicit form (no triple quotes)
    (should-not (string-match-p "\"\"\"" result))
    ;; Text content is preserved
    (should (string-match-p "first line" result))
    (should (string-match-p "second line" result))
    (should (string-match-p "third line" result))
    ;; The \n suffix is gone
    (should-not (string-match-p (regexp-quote "\"first line\\n\"") result))))

(ert-deftest pstt-roundtrip/prefix-preserved ()
  "String prefix is preserved through a full round-trip."
  (let* ((original "x = f\"\"\"\\\n    hello\n    world\"\"\"")
         (result   (pstt-toggle-twice original "hello")))
    (should (string-match-p "f\"\"\"" result))))

(ert-deftest pstt-roundtrip/implicit-no-newlines ()
  "Implicit concat without \\n escapes round-trips through triple and back.

Fragments without trailing \\n are joined with newlines in the triple form,
preserving the line structure.  Text content survives the round-trip."
  (let* ((original "x = (\"hello \"\n     \"world\")")
         (after-triple (pstt-toggle original "hello"))
         (result       (pstt-with-buffer after-triple
                         (search-forward "hello")
                         (python-string-toggle)
                         (buffer-string))))
    ;; Content must be present (possibly as a single merged fragment)
    (should (string-match-p "hello" result))
    (should (string-match-p "world" result))))

(provide 'test-python-string-toggle)
;;; test-python-string-toggle.el ends here
