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
      ;; Eğer her iki terim de aynıysa, eşleşme başarılı
      ((equal term1 term2) nil)
      ;; Eğer term1 bir değişken ise, eşleşme tablosu döndür
      ((and (stringp term1) (char= (char term1 0) #\X))
       (list (list term1 term2)))
      ;; Eğer term2 bir değişken ise, eşleşme tablosu döndür
      ((and (stringp term2) (char= (char term2 0) #\X))
       (list (list term2 term1)))
      ;; Eğer her iki terim de listelerse, eleman eleman birleşim yap
      ((and (listp term1) (listp term2))
       (unify-lists term1 term2))
      ;; Aksi halde birleşim başarısız
      (t nil))))

;; Mantık Çözümleme ve Derinlik-Öncelikli Arama
(defun prolog-prove (axioms query)
  "Prolog çözümleyici. Axioms ve query alır ve çözüm sonucunu döndürür."
  (labels ((resolve (query bindings)
             (let ((results '()))  ;; Çözümleri toplamak için bir liste
               (dolist (axiom axioms)
                 (let* ((parsed-axiom (parse-axiom axiom))
                        (head (getf parsed-axiom :head))
                        (body (getf parsed-axiom :body))
                        (unified (unify query head)))
                   (when unified
                     (let ((new-bindings (append bindings unified)))
                       (if (null body)
                           ;; Eğer body yoksa, bu bir fact, çözümleri topla
                           (push new-bindings results)
                           ;; Aksi halde body'nin her bir elemanını çözümle
                           (let ((sub-results
                                  (mapcar (lambda (subquery)
                                            (resolve subquery new-bindings))
                                          body)))
                             (when (every #'identity sub-results)
                               (push new-bindings results))))))))
               results)))  ;; Tüm çözümleri döndür
    ;; Ana çözüm döngüsü
    (let ((result (resolve query nil)))
      (if result
          (remove-duplicates result :test #'equal)
          nil))))



;; Test: Prolog Prove
(let ((axioms '((("father" "jim" "jill"))
                 (("mother" "mary" "jill"))
                 (("father" "samm" "jim"))
                 (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                 (("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y"))
                 (("parent" "X" "Y") "<" ("mother" "X" "Y"))
                 (("parent" "X" "Y") "<" ("father" "X" "Y"))))
      (query '("ancestor" "X" "jill")))
  (format t "Prolog Prove Result: ~a~%" (prolog-prove axioms query)))
