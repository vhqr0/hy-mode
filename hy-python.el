;;; -*- lexical-binding: t -*-

(require 'python)
(require 'hy-mode)
(require 'thingatpt)

(defconst hy-python-shell-eval-setup-code "
import hy
if not hasattr(hy, hy.mangle('&reader')):
    setattr(hy, hy.mangle('&reader'), hy.reader.hy_reader.HyReader())
hy_reader = getattr(hy, hy.mangle('&reader'))
def __HY_PYTHON_EL_eval(source, filename):
    import sys, io
    if isinstance(source, bytes):
        source = source.decode()
    retval = None
    sio = io.StringIO(source)
    try:
        while True:
            try:
                sexp = hy.read(sio, filename=filename, reader=hy_reader)
            except EOFError:
                return retval
            retval = hy.eval(sexp, locals=globals())
    except Exception:
        t, v, tb = sys.exc_info()
        sys.excepthook(t, v, tb.tb_next)
        return
def __HY_PYTHON_EL_eval_file(filename, tempname, delete):
    import codecs, os, re
    pattern = r'^[ \t\f]*#.*?coding[:=][ \t]*([-_.a-zA-Z0-9]+)'
    with codecs.open(tempname or filename, encoding='latin-1') as file:
        match = re.match(pattern, file.readline())
        match = match or re.match(pattern, file.readline())
        encoding = match.group(1) if match else 'utf-8'
    with codecs.open(tempname or filename, encoding=encoding) as file:
        source = file.read().encode(encoding)
    if delete and tempname:
        os.remove(tempname)
    return __HY_PYTHON_EL_eval(source, filename)
")

(defconst hy-python-shell-ipython-setup-code "
try:
    from IPython.core.magic import register_line_magic
    _ipy_magic_hy = lambda line: __HY_PYTHON_EL_eval(line, '__main__')
    _ipy_magic_hy.__name__ = 'hy'
    register_line_magic(_ipy_magic_hy)
except Exception:
    pass
")

(defun hy-python-shell-first-prompt-hook ()
  (cl-letf (((symbol-function 'python-shell-send-string)
             (lambda (string process)
               (comint-send-string
                process
                (format "exec(%s)\n" (python-shell--encode-string string))))))
    (python-shell-send-string-no-output hy-python-shell-eval-setup-code)
    (python-shell-send-string-no-output hy-python-shell-ipython-setup-code)))



(defun hy-python-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (python-shell-get-process-or-error msg)))
        (code (format "__HY_PYTHON_EL_eval(%s, %s)\n" ;; "__PYTHON_EL_eval(%s, %s)\n"
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

(defun hy-python-shell-send-file (file-name &optional process temp-file-name
                                            delete msg)
  "Send FILE-NAME to inferior Python PROCESS.

If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  FILE-NAME can be remote, but TEMP-FILE-NAME must be
in the same host as PROCESS.  If TEMP-FILE-NAME and DELETE are
non-nil, then TEMP-FILE-NAME is deleted after evaluation is
performed.

When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; temp-file-name
    nil                                 ; delete
    t))                                 ; msg
  (setq process (or process (python-shell-get-process-or-error msg)))
  (with-current-buffer (process-buffer process)
    (unless (or temp-file-name
                (string= (file-remote-p file-name)
                         (file-remote-p default-directory)))
      (setq delete t
            temp-file-name (with-temp-buffer
                             (insert-file-contents file-name)
                             (python-shell--save-temp-file (current-buffer))))))
  (let* ((file-name (file-local-name (expand-file-name file-name)))
         (temp-file-name (when temp-file-name
                           (file-local-name (expand-file-name
                                             temp-file-name)))))
    (comint-send-string
     process
     (format
      "__HY_PYTHON_EL_eval_file(%s, %s, %s)\n" ;; "__PYTHON_EL_eval_file(%s, %s, %s)\n"
      (python-shell--encode-string file-name)
      (python-shell--encode-string (or temp-file-name ""))
      (if delete "True" "False")))))



(defun hy-python-shell-send-string-around (func &rest args)
  (apply (if (eq major-mode 'hy-mode) #'hy-python-shell-send-string func) args))

(defun hy-python-shell-send-file-around (func &rest args)
  (apply (if (eq major-mode 'hy-mode) #'hy-python-shell-send-file func) args))

(defun hy-python-shell-send-defun-around (func arg msg)
  (interactive (list current-prefix-arg t))
  (if (eq major-mode 'hy-mode)
      (python-shell-send-string (thing-at-point 'defun) nil msg)
    (funcall func arg msg)))

(defun hy-python-shell-buffer-substring-around (func start end &rest args)
  (if (eq major-mode 'hy-mode)
      (buffer-substring-no-properties start end)
    (apply func start end args)))

(defun hy-python-macroexpand ()
  (interactive)
  (python-shell-send-string
   (format "(print (hy.repr (hy.macroexpand '%s)))" (thing-at-point 'sexp))))



(defun python-shell-toggle-hy-magic ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (if (looking-at-p "%hy")
        (progn
          (delete-char 3)
          (delete-horizontal-space))
      (delete-trailing-whitespace)
      (insert "%hy ")))
  (when (= (point) (line-beginning-position))
    (goto-char (line-end-position))))



(add-hook 'python-shell-first-prompt-hook #'hy-python-shell-first-prompt-hook)

(advice-add 'python-shell-send-string      :around #'hy-python-shell-send-string-around)
(advice-add 'python-shell-send-file        :around #'hy-python-shell-send-file-around)
(advice-add 'python-shell-send-defun       :around #'hy-python-shell-send-defun-around)
(advice-add 'python-shell-buffer-substring :around #'hy-python-shell-buffer-substring-around)

(define-key hy-mode-map (kbd "C-c C-p") #'run-python)
(define-key hy-mode-map (kbd "C-c C-s") #'python-shell-send-string)
(define-key hy-mode-map (kbd "C-c C-r") #'python-shell-send-region)
(define-key hy-mode-map (kbd "C-c C-c") #'python-shell-send-buffer)
(define-key hy-mode-map (kbd "C-c C-l") #'python-shell-send-file)
(define-key hy-mode-map (kbd "C-c C-z") #'python-shell-switch-to-shell)
(define-key hy-mode-map (kbd "C-M-x")   #'python-shell-send-defun)
(define-key hy-mode-map (kbd "C-c RET") #'hy-python-macroexpand)

(define-key inferior-python-mode-map "\M-h" #'python-shell-toggle-hy-magic)



(provide 'hy-python)
