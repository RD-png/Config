;;; my-doom.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p exp) exp (list exp)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (doom-enlist ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.
Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice.
BINDINGS is either:
  A list of, or a single, `defun', `defun*', `defmacro', or `defadvice' forms.
  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.
TYPE is one of:
  `defun' (uses `cl-letf')
  `defun*' (uses `cl-labels'; allows recursive references),
  `defmacro' (uses `cl-macrolet')
  `defadvice' (uses `defadvice!' before BODY, then `undefadvice!' after)
NAME, ARGLIST, and BODY are the same as `defun', `defun*', `defmacro', and
`defadvice!', respectively.
\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice `(progn (defadvice! ,@rest)
                                  (unwind-protect ,body (undefadvice! ,@rest))))
              ((or `defun `defun*)
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  ,(if (eq type 'defun*)
                       `(cl-labels ((,@rest)) ,body)
                     `(cl-letf (((symbol-function #',(car rest))
                                 (lambda! ,(cadr rest) ,@(cddr rest))))
                        ,body))))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro lambda! (arglist &rest body)
  "Returns (cl-function (lambda ARGLIST BODY...))
The closure is wrapped in `cl-function', meaning ARGLIST will accept anything
`cl-defun' will. Implicitly adds `&allow-other-keys' if `&key' is present in
ARGLIST."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((nlistp (cdr-safe arg)) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         (allow-other-keys arglist))
      ,@body)))

(provide 'my-doom)
