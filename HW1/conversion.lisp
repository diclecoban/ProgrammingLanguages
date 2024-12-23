(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload "cl-ppcre")
(use-package :cl-ppcre)

(defpackage :c-to-lisp-converter
  (:use :cl :cl-ppcre))
(in-package :c-to-lisp-converter)

(defun line-type (line)
  (cond
    ((and (search "if" line) (search "(" line) (search "{" line)) 'if-statement)
    ((and (search "for" line) (search "(" line) (search "{" line)) 'for-loop)
    ((search "return" line) 'return-statement)
    ((and (search "(" line) (search ")" line) (search "{" line)) 'function-definition)
    ((and (search "(" line) (search ")" line) (search ";" line) (not (search "printf" line)) (not (search "=" line))) 'function-declaration)
    ((variable-declaration line) 'variable-declaration)
    ((function-call line) 'function-call)
    ((search "printf" line) 'printf-statement)
    (t 'unknown)))

(defun conversion-foo (line)
  (let ((type (line-type line)))
    (cond
      ((eq type 'if-statement) (convert-if-statement line))
      ((eq type 'for-loop) (convert-for-loop line))
      ((eq type 'return-statement) (convert-return-statement line))
      ((eq type 'function-definition) (convert-function-definition line))
      ((eq type 'variable-declaration) (convert-variable-declaration line))
      ((eq type 'function-call) (convert-function-call line))
      ((eq type 'function-declaration) (convert-function-declaration line))
      ((eq type 'printf-statement) (convert-printf-statement line))
      (t nil))))

(defun convert-function-declaration (line)
  (when (search "int" line)
    (let* ((parts (cl-ppcre:split "\\s+" line))
           (func-name-and-params (second parts))
           (func-name (subseq func-name-and-params 0 (search "(" func-name-and-params))))
      (format nil "(declaim (ftype (function (integer integer) integer) ~a))" func-name))))

(defun convert-if-statement (line)
  (let* ((condition (subseq line (+ (search "(" line) 1) (search ")" line)))
         (condition-split (cl-ppcre:split "\\s+" condition))
         (variable (first condition-split))
         (operator (second condition-split))
         (value (third condition-split)))
    (format nil "(if (~a ~a ~a)" operator variable value)))

(defun variable-declaration (line)
  (cl-ppcre:scan "^\\s*(int|float|double|char)\\s+\\w+\\s*=\\s*.*;\\s*$" line))

(defun function-call (line)
  (and (search "=" line)
       (search "(" line)
       (search ")" line)
       (not (search ";" line))))

(defun convert-function-call (line)
  (let* ((parts (cl-ppcre:split "\\s*=\\s*" line))
         (var-name (first parts))
         (func-call (second parts))
         (clean-func-call (subseq func-call 0 (search ";" func-call))))
    (format nil "(setf ~a ~a)" var-name clean-func-call)))

(defun convert-printf-statement (line)
  (let* ((start (position #\" line))
         (end (and start (position #\" line :start (1+ start))))
         (string (and start end (subseq line (1+ start) end))))
    (if string
        (format nil "(format t \"~a\")" string)
        "(format t \"Error: invalid printf format\")")))

(defun convert-variable-declaration (line)
  (let* ((parts (cl-ppcre:split "\\s*=\\s*" line))
         (left-part (first parts))
         (right-part (second parts))
         (left-split (cl-ppcre:split "\\s+" left-part))
         (var-name (nth 2 left-split))
         (value (subseq right-part 0 (search ";" right-part))))
    (format nil "(setf ~a ~a)" var-name value)))

(defun convert-for-loop (line)
  (let* ((init-expr (subseq line (+ (search "(" line) 1) (search ";" line)))
         (init-expr-split (cl-ppcre:split "\\s+" init-expr))
         (var (second init-expr-split))
         (start-value (fourth init-expr-split))
         (cond-expr (subseq line (+ (search ";" line) 1) (position #\; line :start (+ (search ";" line) 1))))
         (cond-expr-split (cl-ppcre:split "\\s+" cond-expr))
         (comparison-operator (second cond-expr-split))
         (limit (fourth cond-expr-split)))
    (format nil "(loop for ~a from ~a below ~a do)"
            var start-value limit)))

(defun convert-return-statement (line)
  (let* ((expr (subseq line (+ (search "return" line) 7) (search ";" line)))
         (expr-split (cl-ppcre:split "\\s+" expr))
         (operand1 (first expr-split))
         (operator (second expr-split))
         (operand2 (third expr-split)))
    (cond
      ((string= operator "+") (format nil "(+ ~a ~a)" operand1 operand2))
      ((string= operator "-") (format nil "(- ~a ~a)" operand1 operand2))
      ((string= operator "*") (format nil "(* ~a ~a)" operand1 operand2))
      ((string= operator "/") (format nil "(/ ~a ~a)" operand1 operand2))
      (t (format nil "( ~a)" expr)))))

(defun convert-function-definition (line)
  (let* ((func-name-end (position #\( line))
         (func-name-start (position #\space line :from-end t :end func-name-end))
         (func-name (subseq line (1+ func-name-start) func-name-end))
         (params-start (position #\( line))
         (params-end (position #\) line))
         (params (when (and params-start params-end (< params-start params-end))
                   (subseq line (1+ params-start) params-end)))
         (params-split (cl-ppcre:split "[,\\s]+" params))
         (param-names (loop for i from 1 below (length params-split) by 2
                            collect (nth i params-split))))
    (when (and func-name params)
      (format nil "(defun ~a (~{~a~^ ~}))" func-name param-names))))

(defun convert-type (c-type)
  (cond
    ((string= c-type "int") "integer")
    ((string= c-type "void") "void")
    ((string= c-type "double") "double-float")
    ((string= c-type "float") "float")
    ((string= c-type "char") "character")
    ((string= c-type "string") "string")
    (t c-type)))

(defun convert-line (lines)
  (if (null lines)
      nil
      (cons (conversion-foo (first lines)) (convert-line (rest lines)))))

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line collect line)))

(defun write-file (filename lines)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (loop for line in lines do
          (when (not (string= line ""))
            (write-line line stream)))))

(defun main ()
  (let ((lines (read-file "input.c")))
    (let ((converted-lines (convert-line lines)))
      (write-file "dicleOutput.lisp" (remove nil converted-lines)))))

(main)