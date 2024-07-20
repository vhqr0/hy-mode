;;; hy-mode.el --- Yet another major mode for Hylang -*- lexical-binding: t -*-

;; Author: VHQR <zq_cmd@163.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages
;; URL: https://github.com/vhqr0/hy-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Provide `hy-mode' for Hylang with syntax table, highlight,
;; indentation, imenu, and shell support.

;;; Code:

(require 'cl-lib)
(require 'python)

;;;; Names

(defconst hy-python-names
  ;; 2024-07-19
  ;; https://docs.python.org/3/reference/lexical_analysis.html#identifiers
  ;; https://docs.python.org/3/reference/lexical_analysis.html#operators
  ;; https://docs.python.org/3/library/
  '(
    ;;; Operators
    "!=" "%" "%=" "&" "&=" "*" "**" "**=" "*=" "+" "+=" "," "-" "-="
    "/" "//" "//=" "/=" "<" "<<" "<<=" "<=" "=" ">" ">=" ">>" ">>="
    "^" "^=" "|" "|=" "~"
    ;;; Keywrods
    "_" "self" "cls" "False" "None" "True" "NotImplemented" "Ellipsis"
    "and" "as" "assert" "async" "await" "break" "class" "continue"
    "def" "del" "elif" "else" "except" "finally" "for" "from" "global"
    "if" "import" "in" "is" "lambda" "nonlocal" "not" "or" "pass"
    "raise" "return" "try" "while" "with" "yield"
    ;;; Functions
    "abs" "aiter" "all" "anext" "any" "ascii" "bin" "bool" "breakpoint" "bytearray" "bytes"
    "callable" "chr" "classmethod" "compile" "complex" "delattr" "dict" "dir" "divmod"
    "enumerate" "eval" "exec" "filter" "float" "format" "frozeset" "getattr" "globals"
    "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance" "issubclass" "iter"
    "len" "list" "locals" "map" "max" "memoryview" "min" "next" "object" "oct" "open" "ord"
    "pow" "print" "property" "range" "repr" "reversed" "round"
    "set" "setattr" "slice" "sorted" "staticmethod" "str" "sum" "supper"
    "tuple" "type" "vars" "zip"
    ;;; Exceptions
    "BaseException" "Exception" "ArithmeticError" "BufferError" "LookupError"
    "AssertionError" "AttributeError" "EOFError" "FloatingPointError" "GeneratorExit"
    "ImportError" "ModuleNotFoundError" "IndexError" "KeyboardInterrupt" "MemoryError"
    "NameError" "NotImplementedError" "OSError" "OverflowError" "RecursivionError"
    "ReferenceError" "RuntimeError" "StopIteration" "AsyncStopIteration" "SyntaxError"
    "IndentationError" "SystemError" "SystemExit" "TypeError" "UnboundLocalError"
    "UnicodeError" "UnicodeEncodeError" "UnicodeDecodeError" "UnicodeTranslateError"
    "ValueError" "ZeroDivisionError" "EnvironmentError" "IOError" "WindowsError"
    "ExceptionGroup" "BaseExceptionGroup")
  "Names of Python.")

(defconst hy-hylang-names
  ;; 2024-07-19
  ;; https://hylang.org/hy/doc/v0.29.0/api
  ;; https://hylang.org/hyrule/doc/v0.6.0
  '(
    ;;; Fundamentals
    "do" "do-mac" "eval-and-compile" "eval-when-compile" "py" "pys" "pragma"
    ;;; Quoting
    "quote" "quasiquote" "unquote" "unquote-splice"
    ;;; Assignment, mutation, and annotation
    "setv" "setx" "let" "global" "nonlocal" "del" "annotate" "#^" "deftype"
    ;; TODO: forget this?
    "local"
    ;;; Subsetting
    "." "#*" "#**" "unpack-iterable" "unpack-mapping"
    ;;; Conditionals and basic loops
    "if" "when" "cond" "while" "break" "continue" "else"
    ;;; Comprehensions
    "for" "lfor" "dfor" "gfor" "sfor"
    ;;; Context managers and pattern-matching
    "with" "match"
    ;;; Exception-handling
    "raise" "try" "except" "except*" "finally"
    ;;; Functions
    "defn" "fn" "return" "yield" "await"
    ;;; Macros
    "defmacro" "defreader" "get-macro" "local-macros"
    ;;; Classes
    "defclass"
    ;;; Modules
    "import" "require" "export"
    ;;; Miscellany
    "chainc" "assert"
    ;;; Hy
    "read" "read-many" "eval" "repr" "repr-register" "mangle" "unmangle" "macroexpand"
    "macroexpand-1" "gensym" "as-model" "hy.I" "hy.R"
    ;;; Anaphoric
    "#%" "ap-if" "ap-each" "ap-each-while" "ap-map" "ap-map-when" "ap-filter"
    "ap-reject" "ap-dotimes" "ap-first" "ap-last" "ap-reduce" "ap-when" "ap-with"
    ;;; Argmove
    "->" "->>" "as->" "some->" "doto"
    ;;; Collections
    "assoc" "ncut" "postwalk" "prewalk" "#s" "walk"
    ;;; Control
    "block" "block-ret" "block-ret-from" "branch" "case" "cfor" "defmain" "do-n"
    "ebranch" "ecase" "lif" "list-n" "loop" "unless"
    ;;; Destructure
    "defn+" "defn/a+" "dict=:" "fn+" "fn/a+" "let+" "setv+"
    ;;; Iterables
    "butlast" "coll?" "distinct" "drop-last" "flatten" "rest" "thru"
    ;;; Macrotools
    "#/" "defmacro-kwargs" "defmacro!" "macroexpand-all"
    "map-model" "match-fn-params" "with-gensym"
    ;;; Pretty print
    "PrettyPrinter" "pformat" "pp" "pprint" "readable?" "recursive?" "saferepr"
    ;;; Sequence
    "Sequence" "seq" "defseq" "end-sequence"
    ;;; Misc
    "comment" "constantly" "dec" "inc" "import-path" "of" "parse-args"
    "profile/calls" "profile/cpu" "sign" "smacrolet" "xor")
  "Names of Hylang.")

;;;; Shell setup codes

(defconst hy-shell-setup-code "
def __HY_EL_eval(source, filename='__main__'):
    import hy, sys
    if not hasattr(hy, 'hyx_XampersandXreader'):
        hy.hyx_XampersandXreader = hy.reader.hy_reader.HyReader()
    if isinstance(source, bytes):
        source = source.decode()
    try:
        retval = None
        for sexp in hy.read_many(source, filename=filename, reader=hy.hyx_XampersandXreader):
            retval = hy.eval(sexp, locals=globals())
        return retval
    except Exception:
        t, v, tb = sys.exc_info()
        sys.excepthook(t, v, tb.tb_next)
        return
try:
    from IPython.core.magic import register_line_magic
    register_line_magic('hy')(__HY_EL_eval)
except Exception:
    pass
"
  "Setup code to eval Hylang sources.")

;;;; Syntax table

(defconst hy-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\~ "'"  table)
    (modify-syntax-entry ?\, "_"  table)
    (modify-syntax-entry ?\| "_"  table)
    (modify-syntax-entry ?\# "_"  table)
    (modify-syntax-entry ?\@ "_"  table)
    table)
  "Syntax table of `hy-mode'.")

;;;; Highlight

(defconst hy-font-lock-name-keywords
  (list
   (rx-to-string
    `(: symbol-start (or ,@hy-python-names ,@hy-hylang-names) symbol-end))
   '(0 font-lock-builtin-face))
  "Font lock keywords for Python and Hylang names.")

(defconst hy-font-lock-keyword-keywords
  (list
   (rx symbol-start ":" (1+ word))
   '(0 font-lock-constant-face))
  "Font lock keywords for Hylang keywords (starts with :).")

(defconst hy-font-lock-shebang-keywords
  (list
   (rx buffer-start "#!" (0+ not-newline) eol)
   '(0 font-lock-comment-face))
  "Font lock keywords for #!.")

(defconst hy-font-lock-keywords
  (list hy-font-lock-name-keywords
        hy-font-lock-keyword-keywords
        hy-font-lock-shebang-keywords)
  "Font lock keywords of `hy-mode'.")

;;;; Indentation

(defun hy-indent-function (_point syntax)
  "Indent function of `hy-mode'.
_POINT and SYNTAX see `lisp-indent-line'."
  (let ((innermost-char (nth 1 syntax)))
    (when innermost-char
      (goto-char (1+ innermost-char))))
  (if (or (not (eq (char-before) ?\())
          (eq (char-before (1- (point))) ?\#))
      (current-column)
    (1+ (current-column))))

;;;; Imenu

(defun hy-match-next-def (regexp)
  "Match next def by REGEXP."
  (when (re-search-backward regexp nil t)
    (save-excursion
      (let ((point (point)))
        (ignore-errors
          (down-list))
        (forward-sexp)
        (skip-chars-forward " \t")
        ;; skip decorators
        (when (memq (following-char) '(?\( ?\[ ?\{))
          (forward-sexp)
          (skip-chars-forward " \t"))
        ;; skip annotate
        (when (looking-at "#^")
          (forward-sexp 2)
          (skip-chars-forward " \t"))
        (when-let (def (bounds-of-thing-at-point 'symbol))
          (cl-destructuring-bind (def-beg . def-end) def
            (set-match-data (list def-beg def-end))
            (goto-char point)))))))

(defun hy-match-next-global-def ()
  "Imenu global index function for `hy-mode'."
  (hy-match-next-def "^(def"))

(defun hy-match-next-local-def ()
  "Imenu local index function for `hy-mode'."
  (hy-match-next-def "^[ \t]+(def"))

;;;; Shell

(defun hy-shell-send-setup-code ()
  "Send `hy-shell-setup-code' to python shell."
  (cl-letf (((symbol-function 'python-shell-send-string)
             (lambda (string process)
               (comint-send-string
                process
                (format "exec(%s)\n" (python-shell--encode-string string))))))
    (python-shell-send-string-no-output hy-shell-setup-code)))

(add-hook 'python-shell-first-prompt-hook #'hy-shell-send-setup-code)

;; COPY FROM PYTHON.EL 0.28
(defun hy-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Hy command: ") nil t))
  (let ((process (or process (python-shell-get-process-or-error msg)))
        (code (format "__HY_EL_eval(%s, %s)\n"
                      (python-shell--encode-string string)
                      (python-shell--encode-string (or (buffer-file-name)
                                                       "<string>")))))
    (unless python-shell-output-filter-in-progress
      (with-current-buffer (process-buffer process)
        (save-excursion
          (goto-char (process-mark process))
          (insert-before-markers "\n"))))
    (if (or (null (process-tty-name process))
            (<= (string-bytes code)
                (or (bound-and-true-p comint-max-line-length)
                    1024))) ;; For Emacs < 28
        (comint-send-string process code)
      (let* ((temp-file-name (with-current-buffer (process-buffer process)
                               (python-shell--save-temp-file string)))
             (file-name (or (buffer-file-name) temp-file-name)))
        (python-shell-send-file file-name process temp-file-name t)))))

(defun hy-shell-around-send-string (func &rest args)
  "Around for `python-shell-send-string' to use `hy-shell-send-string'.
FUNC and ARGS see `python-shell-send-string'."
  (let ((send-func
         (if (eq major-mode 'hy-mode) #'hy-shell-send-string func)))
    (apply send-func args)))

(defun hy-shell-around-send-defun (func &rest args)
  "Around for `python-shell-send-defun' to use `thing-at-point'.
FUNC and ARGS see `python-shell-send-defun'."
  (if (eq major-mode 'hy-mode)
      (hy-shell-send-string (or (thing-at-point 'defun) (thing-at-point 'sexp)))
    (apply func args)))

(defun hy-shell-around-send-statement (func &rest args)
  "Around for `python-shell-send-statement' to use `thing-at-point'.
FUNC and ARGS see `python-shell-send-statement'."
  (if (eq major-mode 'hy-mode)
      (hy-shell-send-string (thing-at-point 'sexp))
    (apply func args)))

(defun hy-shell-around-buffer-substring (func start end &rest args)
  "Around for `python-shell-buffer-substring' to use `buffer-substring'.
FUNC, START, END and ARGS see `python-shell-buffer-substring'."
  (if (eq major-mode 'hy-mode)
      (buffer-substring-no-properties start end)
    (apply func start end args)))

(advice-add 'python-shell-send-string :around #'hy-shell-around-send-string)
(advice-add 'python-shell-send-defun :around #'hy-shell-around-send-defun)
(advice-add 'python-shell-send-statement :around #'hy-shell-around-send-statement)
(advice-add 'python-shell-buffer-substring :around #'hy-shell-around-buffer-substring)

(defun hy-shell-macroexpand ()
  "Expand macro at point."
  (interactive)
  (hy-shell-send-string
   (format "(print (hy.repr (hy.macroexpand '%s)))" (thing-at-point 'sexp))))

;;;; Keymap

(defvar-keymap hy-mode-map
  :doc "Keymap for `hy-mode'."
  :parent lisp-mode-shared-map
  "C-c C-p" #'run-python
  "C-c C-z" #'python-shell-switch-to-shell
  "C-c C-s" #'python-shell-send-string
  "C-c C-r" #'python-shell-send-region
  "C-c C-c" #'python-shell-send-buffer
  "C-c C-e" #'python-shell-send-statement
  "C-c C-l" #'python-shell-send-file
  "C-M-x" #'python-shell-send-defun
  "C-c RET" #'hy-shell-macroexpand
  "C-c C-RET" #'hy-shell-macroexpand)

;;;; Major mode

;;;###autoload
(define-derived-mode hy-mode prog-mode "Hy"
  "Major mode for editing Hy files."

  (set-syntax-table hy-mode-syntax-table)

  (setq-local comment-add 1)
  (setq-local comment-start ";")
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")

  (setq-local font-lock-multiline t)
  (setq-local font-lock-defaults
              '(hy-font-lock-keywords
                nil nil nil nil
                (font-lock-mark-block-function . mark-defun)))

  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'hy-indent-function)

  (setq-local imenu-generic-expression
              '(("Local" hy-match-next-local-def 0)
                ("Global" hy-match-next-global-def 0))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("hy" . hy-mode))

(provide 'hy-mode)
;;; hy-mode.el ends here
