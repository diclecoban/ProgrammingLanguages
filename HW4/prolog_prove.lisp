(defun is-variable (arg)
  "Checks if an argument is a variable."
  (and (stringp arg) (every #'alpha-char-p arg) (upper-case-p (char arg 0))))

(defun unify (query axiom bindings)
  "Unifies a query with an axiom and updates variable bindings if successful."
  (format t "Unifying ~a with ~a~%" query axiom)
  (if (and (equal (first query) (first axiom)) ; Predikat isimleri eşleşiyor mu?
           (= (length query) (length axiom))) ; Argüman sayıları eşit mi?
      (let ((local-bindings (copy-list bindings))) ; Bağlamların bir kopyasını kullan
        (loop for q in (rest query)
              for a in (rest axiom)
              do (cond
                   ((string= q a) nil) ; Sabitler eşleşirse devam et
                   ((and (is-variable q) (not (assoc q local-bindings)))
                    (push (cons q a) local-bindings)
                    (format t "Binding added: ~a -> ~a~%" q a))
                   ((assoc q local-bindings)
                    (if (not (string= (cdr (assoc q local-bindings)) a))
                        (progn
                          (format t "Conflict: ~a -> ~a does not match ~a~%" q (cdr (assoc q local-bindings)) a)
                          (return nil))))))
        (format t "Bindings after unify: ~a~%" local-bindings)
        local-bindings) ; Güncellenmiş bağlamları döndür
      (progn
        (format t "Failed to unify: ~a with ~a~%" query axiom)
        nil))) ; Başarısız olursa nil döndür

(defun prolog_prove (axioms query)
  "Proves a query using the provided axioms."
  (format t "Proving query: ~a~%" query)
  (labels
      ((solve (query bindings)
         (if (null query) ; Eğer sorgu boşsa, başarılı çözüm
             bindings
             (let ((current (first query))
                   (remaining (rest query)))
               (loop for axiom in axioms
                     do (let ((result (if (member "<" axiom)
                                          (apply-rule axiom current bindings) ; Koşullu aksiyon
                                          (unify current (first axiom) bindings)))) ; Basit aksiyon
                          (if result
                              (progn
                                (format t "Success: Moving to next query ~a with bindings ~a~%" remaining result)
                                (return (solve remaining result)))))) ; Alt sorgulara geç
               (format t "Failed to solve query: ~a with bindings ~a~%" query bindings)
               nil)))

       (apply-rule (rule query bindings)
         "Applies a rule to a query and returns updated bindings if successful."
         (format t "Applying rule: ~a to query: ~a~%" rule query)
         (let ((head (first rule))
               (body (rest (rest rule)))) ; Koşullu aksiyonun gövdesi
           (let ((new-bindings (unify query head bindings)))
             (if new-bindings
                 (progn
                   (format t "Rule applied successfully. New bindings: ~a~%" new-bindings)
                   (solve body new-bindings)) ; Alt sorguları çöz
                 (progn
                   (format t "Rule application failed.~%")
                   nil)))))) ; Başarısızlık durumunda nil döndür
    (solve query nil))) ; Başlangıç bağlaması boş

;; Test
(let ((axioms '( (("father" "jim" "jill"))
                 (("ancestor" "X" "Y") "<" (("father" "X" "Y"))) )))
  (print (prolog_prove axioms '(("ancestor" "X" "jill")))))
