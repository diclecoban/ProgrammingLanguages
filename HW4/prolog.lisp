;; Kural ve Sorgu Yapısı
(defun parse-axiom (axiom)
  "Bir axiom listesini head ve body olarak ayırır."
  (if (and (listp axiom) (member '< axiom))
      (let ((head (car axiom))
            (body (cddr axiom)))
        (list :head head :body body))
      (list :head (car axiom) :body nil)))

(defun parse-query (query)
  "Bir sorguyu alır ve işlenebilir hale getirir."
  (car query))

;; Birleşim İşlemi (Unification)
(defun unify (term1 term2)
  "İki terimi birleştirir ve eşleşme tablosu döndürür."
  (labels ((unify-lists (list1 list2)
             "İki listeyi eleman eleman birleştirir."
             (if (or (null list1) (null list2))
                 nil
                 (let ((first-unify (unify (car list1) (car list2)))
                       (rest-unify (unify-lists (cdr list1) (cdr list2))))
                   (if (and first-unify rest-unify)
                       (append first-unify rest-unify)
                       (or first-unify rest-unify))))))
    (cond
      ((equal term1 term2) nil) ;; Terimler eşleşiyor
      ((and (stringp term1) (string= (subseq term1 0 1) "X"))
       (list (list term1 term2))) ;; Term1 bir değişken
      ((and (stringp term2) (string= (subseq term2 0 1) "X"))
       (list (list term2 term1))) ;; Term2 bir değişken
      ((and (listp term1) (listp term2))
       (unify-lists term1 term2)) ;; Liste birleşimi
      (t nil)))) ;; Başarısız birleşim

(defun filter-invalid-bindings (bindings axiom relation)
  "Uygun olmayan bağlamları çıkarır."
  (remove-if
   (lambda (binding)
     (and (equal relation "uncle")
          (equal (second binding) (third (first axiom)))))
   bindings))

(defun prolog-prove (axioms queries)
  "Prolog çözümleyici."
  (labels ((resolve (query bindings)
             (let ((results '()))
               (dolist (axiom axioms)
                 (let* ((parsed-axiom (parse-axiom axiom))
                        (head (getf parsed-axiom :head))
                        (body (getf parsed-axiom :body))
                        (unified (unify query head)))
                   (when (and unified (not (null unified)))
                     (let ((new-bindings (append bindings unified)))
                       (if (null body)
                           (push (filter-invalid-bindings new-bindings axiom (first query)) results)
                           (let ((sub-results (mapcan (lambda (subquery) (resolve subquery new-bindings)) body)))
                             (when sub-results
                               (push (append unified sub-results) results))))))))
               results)))
    (mapcan (lambda (query) (resolve query nil)) queries)))

;; Testler
(format t "Test 1 Result: ~a~%"
         (prolog-prove
          '((("father" "jim" "jill"))
            (("mother" "mary" "jill")))
          '(("parent" "X" "jill"))))

(format t "Test 2 Result: ~a~%"
         (prolog-prove
          '((("sibling" "samm" "jim"))
            (("father" "jim" "jill")))
          '(("uncle" "X" "jill"))))

(format t "Test 3 Result: ~a~%"
    (prolog-prove
 '((("father" "bob" "alice"))
   (("sibling" "samm" "bob"))
   (("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y")))
 '(("uncle" "X" "alice"))))







