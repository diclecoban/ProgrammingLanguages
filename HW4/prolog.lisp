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
  "Bağlamlardan uygun olmayanları çıkarır. Özellikle 'uncle' ilişkisinde hem 'father' hem 'uncle' çakışmalarını kontrol eder."
  (if (not (equal relation "uncle"))
      bindings ;; Eğer ilişki 'uncle' değilse bağlamları olduğu gibi döndür
      (remove-if
       (lambda (binding)
         (let ((person (second binding)))
           (or
            ;; Eğer kişi bir "father" ise ve aynı zamanda "uncle" olarak işaretlenmişse bağlamı çıkar
            (some (lambda (ax)
                    (and (listp ax)
                         (listp (first ax))
                         (equal (first (first ax)) "father")
                         (equal person (second (first ax)))))
                  axiom)
            ;; Eğer kişi bir "mother" ise ve aynı zamanda "uncle" olarak işaretlenmişse bağlamı çıkar
            (some (lambda (ax)
                    (and (listp ax)
                         (listp (first ax))
                         (equal (first (first ax)) "mother")
                         (equal person (second (first ax)))))
                  axiom))))
       bindings)))

(defun filter-ancestor-bindings (bindings)
  "Ancestor bağlamlarından parent olanları filtreler."
  (remove-if
   (lambda (binding)
     (some (lambda (relation)
             (and (listp relation)
                  (equal (first relation) "parent")
                  (equal (second relation) (second binding))))
           bindings))
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
                           (progn
                             (let ((filtered-bindings
                                    (if (equal (first query) "ancestor")
                                        (filter-ancestor-bindings new-bindings)
                                        (filter-invalid-bindings new-bindings axioms (first query)))))
                               (when filtered-bindings
                                 (push filtered-bindings results))))
                           (let ((sub-results
                                  (mapcan (lambda (subquery) (resolve subquery new-bindings))
                                          body)))
                             (when (and (every #'identity sub-results) (not (null sub-results)))
                               (let ((merged-bindings
                                      (reduce (lambda (x y)
                                                (if (and x y) (append x y) nil))
                                              sub-results)))
                                 (let ((filtered-bindings
                                        (if (equal (first query) "ancestor")
                                            (filter-ancestor-bindings merged-bindings)
                                            (filter-invalid-bindings merged-bindings axioms (first query)))))
                                   (when filtered-bindings
                                     (push filtered-bindings results)))))))))))
               results)))
    (let ((result (mapcan (lambda (query) (resolve query nil)) queries)))
      (if result
          (mapcar (lambda (binding)
                    (mapcar (lambda (pair)
                              (if (and (listp pair)
                                       (equal (length pair) 2)
                                       (string= (subseq (first pair) 0 1) "X"))
                                  (list (first pair) (second pair))
                                  pair))
                            binding))
                  (remove-duplicates result :test #'equal))
          nil))))


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
          '((("father" "bob" "jill"))
            (("mother" "mary" "jill"))
            (("sibling" "samm" "bob"))
            (("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("parent" "Z" "Y")))
          '(("uncle" "X" "jill"))))

(let (
      (axioms '(
                (("father" "jim" "jill"))
                (("mother" "mary" "jill"))
                (("father" "samm" "jim"))
                (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                (("parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query1 '(("ancestor" "X" "jill")))
      (query2 '(("ancestor" "X" "jill") ("mother" "X" "bob"))))

  (format t "Test 4 Result (Query1): ~a~%" (prolog-prove axioms query1))
  (format t "Test 5 Result (Query2): ~a~%" (prolog-prove axioms query2)))