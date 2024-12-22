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
    (format t "Attempting to unify ~a with ~a~%" term1 term2)
    (cond
      ;; Eğer her iki terim de aynıysa, eşleşme başarılı
      ((equal term1 term2)
       (format t "Terms match directly: ~a~%" term1)
       nil)
      ;; Eğer term1 bir değişken ise, eşleşme tablosu döndür
      ((and (stringp term1) (string= (subseq term1 0 1) "X"))
       (format t "Binding variable ~a to ~a~%" term1 term2)
       (list (list term1 term2)))
      ;; Eğer term2 bir değişken ise, eşleşme tablosu döndür
      ((and (stringp term2) (string= (subseq term2 0 1) "X"))
       (format t "Binding variable ~a to ~a~%" term2 term1)
       (list (list term2 term1)))
      ;; Eğer her iki terim de listelerse, eleman eleman birleşim yap
      ((and (listp term1) (listp term2))
       (format t "Attempting to unify lists: ~a with ~a~%" term1 term2)
       (unify-lists term1 term2))
      ;; Aksi halde birleşim başarısız
      (t
       (format t "Unification failed for ~a and ~a~%" term1 term2)
       nil))))

;; Mantık Çözümleme ve Derinlik-Öncelikli Arama
(defun prolog-prove (axioms queries)
  "Prolog çözümleyici. Axioms ve queries alır ve çözüm sonucunu döndürür."
  (labels ((resolve (query bindings)
             (let ((results '()))  ;; Çözümleri toplamak için bir liste
               (dolist (axiom axioms)
                 (let* ((parsed-axiom (parse-axiom axiom))
                        (head (getf parsed-axiom :head))
                        (body (getf parsed-axiom :body))
                        (unified (unify query head)))
                   (format t "Checking Axiom: ~a~%" axiom) ;; Hata ayıklama mesajı
                   (format t "Unified Result: ~a~%" unified) ;; Hata ayıklama mesajı
                   (when (and unified (not (null unified)))  ;; Bağlamın NIL olmadığını kontrol et
                     (let ((new-bindings (append bindings unified)))
                       (format t "New Bindings: ~a~%" new-bindings) ;; Hata ayıklama mesajı
                       (if (null body)
                           ;; Eğer body yoksa, bu bir fact, çözümleri topla
                           (progn
                             (format t "Fact found, adding bindings: ~a~%" new-bindings) ;; Hata ayıklama mesajı
                             (push new-bindings results))
                           ;; Body alt sorgularını çöz
                           (let ((sub-results
                                  (mapcar (lambda (subquery)
                                            (resolve subquery new-bindings))
                                          body)))
                             ;; Eğer tüm alt sorgular çözüldüyse ve bağlam NIL değilse, sonucu ekle
                             (when (and (every #'identity sub-results)
                                        (not (null sub-results)))
                               (let ((merged-bindings (reduce (lambda (x y)
                                                               (if (and x y) (append x y) nil))
                                                             sub-results)))
                                 (when (not (null merged-bindings)) ;; NIL sonuçları filtrele
                                   (format t "Sub-query resolved: ~a~%" merged-bindings) ;; Hata ayıklama mesajı
                                   (push merged-bindings results))))))))))
               results)))
    ;; Ana çözüm döngüsü
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
                  (remove-duplicates result :test #'equal)) ;; Tüm NIL bağlamları kaldır
          nil))))

;; Test: Prolog Prove
(format t "Result of this  ~a~%" (prolog-prove
                                  '((("father" "bob" "alice"))
                                    (("sibling" "samm" "bob"))
                                    (("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y")))
                                  '(("uncle" "X" "alice")))
)

