(load "gpp_lexer.lisp")

(defun tokenize (input)
  (mapcar #'categorize-token (split-text input)))


(defun cfg-validate (tokens)
  (when (and (equal (first tokens) 'OP_OP)
             (equal (last tokens) 'OP_CP)) 
      (cond
        ;; KW_AND, KW_OR, KW_NOT
        ((and (member operator '(KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS))
              (every #'cfg-validate args)) t)
        ;; KW_LIST, KW_APPEND, KW_CONCAT
        ((and (member operator '(KW_LIST KW_APPEND KW_CONCAT))
              (every #'cfg-validate args)) t)
        ;; Arithmetic Operations
        ((and (member operator '(OP_PLUS OP_MINUS OP_MUL OP_DIV))
              (every #'numberp args)) t)
        ;; VALUEI, VALUEF, IDENTIFIER, KW_TRUE, KW_FALSE, KW_NIL
        ((or (member operator '(VALUEI VALUEF IDENTIFIER KW_TRUE KW_FALSE KW_NIL))) t)
        ;; iç paranthesis
        ((and (equal (first args) 'OP_OP)
              (equal (last args) 'OP_CP)
              (cfg-validate (subseq args 1 (1- (length args))))) t)
        (t nil))))



;;(defun parse (tokens)
;;  (let ((token (pop tokens)))
;;    (cond
;;      ((string= token "(")
;;       (let ((operator (pop tokens))
;;             (args '()))
;;         (loop while (not (string= (first tokens) ")"))
;;               do (push (parse tokens) args))
;;         (pop tokens)
;;         (cons operator (reverse args))))
;;      ((every #'digit-char-p token)
;;       (parse-integer token))
;;      (t token))))


;;(defun evaluate (parsed)
;;  (cond
;;    ((listp parsed)
;;     (let ((operator (first parsed))
;;           (args (rest parsed)))
;;       (case operator
;;         (+ (apply #'+ (mapcar #'evaluate args)))
;;         (- (apply #'- (mapcar #'evaluate args)))
;;         (* (apply #'* (mapcar #'evaluate args)))
;;         (/ (apply #'/ (mapcar #'evaluate args)))
;;         (if (if-evaluator (first args) (second args)))
;;         (set (set-evaluator (first args) (second args)))
;;         (t (error "Unknown işlem: ~a" operator)))))
;;   ((numberp parsed) parsed)
;;    (t parsed)))

(defun gppinterpreter (&optional file)
  (if file
      (progn
        (format t "Loading file: ~a~%" file)
        (process-file file))
      (interactive-mode)))

(defun process-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          unless (starts-with ";;" line)
          do (handle-input line))))


(defun handle-input (input)
  (let* ((tokens (tokenize input)))
    (if (cfg-validate tokens)
        (format t "Input: ~a~%Tokens: ~{~a~^ ~}~%Valid Syntax!~%~%" input tokens)
        (format t "Input: ~a~%Tokens: ~{~a~^ ~}~%Error: Invalid Syntax!~%~%" input tokens))))
