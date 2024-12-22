;; I use parsing the axioms to maintain them easily
(defun parse-axiom (axiom)
  (if (and (listp axiom) (member '< axiom))
      (let ((head (car axiom))
            (body (cddr axiom)))
        (list :head head :body body))
      (list :head (car axiom) :body nil)))


(defun parse-query (query)
  (car query))

;; it merge them (birleştirir)
(defun unify (term1 term2)
  (labels ((unify-lists (list1 list2)
             (if (or (null list1) (null list2))
                 nil
                 (let ((first-unify (unify (car list1) (car list2)))
                       (rest-unify (unify-lists (cdr list1) (cdr list2))))
                   (if (and first-unify rest-unify)
                       (append first-unify rest-unify)
                       (or first-unify rest-unify))))))
    (cond
      ((equal term1 term2) nil) ;; terms are the same
      ((and (stringp term1) (string= (subseq term1 0 1) "X"))
       (list (list term1 term2))) ;; term1 is a değişken
      ((and (stringp term2) (string= (subseq term2 0 1) "X"))
       (list (list term2 term1))) ;; term2 is a değişken
      ((and (listp term1) (listp term2))
       (unify-lists term1 term2)) ;; lists merging
      (t nil)))) ;; failed merging

(defun filter-uncle-bindings (bindings axiom relation)
  (if (not (equal relation "uncle"))
      bindings ;; if it's not uncle return what I have
      (remove-if
       (lambda (binding)
         (let ((person (second binding)))
           (or
            ;; if someone marked as "uncle" but its also "father" dont return that
            (some (lambda (ax)
                    (and (listp ax)
                         (listp (first ax))
                         (equal (first (first ax)) "father")
                         (equal person (second (first ax)))))
                  axiom)
               ;; if someone marked as "uncle" but its also "mother" dont return that. because of the parent situation I need to add that condition
            (some (lambda (ax)
                    (and (listp ax)
                         (listp (first ax))
                         (equal (first (first ax)) "mother")
                         (equal person (second (first ax)))))
                  axiom))))
       bindings)))

(defun filter-aunt-bindings (bindings axiom relation)
  (if (not (equal relation "aunt"))
      bindings ;; if it's not aunt return what I have
      (remove-if
       (lambda (binding)
         (let ((person (second binding)))
           (or
            ;; if someone marked as "aunt" but its also "father" dont return that
            (some (lambda (ax)
                    (and (listp ax)
                         (listp (first ax))
                         (equal (first (first ax)) "father")
                         (equal person (second (first ax)))))
                  axiom)
               ;; if someone marked as "aunt" but its also "mother" dont return that. because of the parent situation I need to add that condition
            (some (lambda (ax)
                    (and (listp ax)
                         (listp (first ax))
                         (equal (first (first ax)) "mother")
                         (equal person (second (first ax)))))
                  axiom))))
       bindings)))


(defun filter-ancestor-bindings (bindings)
  (remove-if
   (lambda (binding)
     (some (lambda (relation)
             (and (listp relation)
                  (equal (first relation) "parent")
                  (equal (second relation) (second binding))))
           bindings))
   bindings))

(defun is-parent (axioms parent child)
  (or (some (lambda (ax) (and (equal (first ax) "father")
                              (equal (second ax) parent)
                              (equal (third ax) child))) axioms)
      (some (lambda (ax) (and (equal (first ax) "mother")
                              (equal (second ax) parent)
                              (equal (third ax) child))) axioms)))



(defun prolog-prove (axioms queries)
  (labels ((resolve (query bindings)
             (let ((results '()))
               (dolist (axiom axioms)
                 (let* ((parsed-axiom (parse-axiom axiom))
                        (head (getf parsed-axiom :head))
                        (body (getf parsed-axiom :body))
                        (unified (unify query head)))
                   (when (and (equal (first query) "parent")
                              (is-parent axioms (second query) (third query)))
                     (push (append bindings (list (list "parent" (second query) (third query)))) results))
                   (when (and unified (not (null unified)))
                     (let ((new-bindings (append bindings unified)))
                       (if (null body)
                           (progn
                             (let ((filtered-bindings
                                    (cond
                                     ((equal (first query) "parent") new-bindings)
                                     ((equal (first query) "ancestor")
                                      (filter-ancestor-bindings new-bindings))
                                     ((equal (first query) "uncle")
                                      (filter-uncle-bindings new-bindings axioms (first query)))
                                     ((equal (first query) "aunt")
                                      (filter-aunt-bindings new-bindings axioms (first query)))
                                     (t new-bindings))))
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
                                        (cond
                                         ((equal (first query) "parent") merged-bindings)
                                         ((equal (first query) "ancestor")
                                          (filter-ancestor-bindings merged-bindings))
                                         ((equal (first query) "uncle")
                                          (filter-uncle-bindings merged-bindings axioms (first query)))
                                         ((equal (first query) "aunt")
                                          (filter-aunt-bindings merged-bindings axioms (first query)))
                                         (t merged-bindings))))
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



;; Tests
(format t "Test 1 Result: ~a~%"
         (prolog-prove
          '((("father" "jim" "jill"))
            (("mother" "mary" "juliana")))
          '(("parent" "X" "jill"))))

(format t "Test 2 Result: ~a~%"
         (prolog-prove
          '((("sibling" "jane" "jim"))
            (("father" "jim" "jill")))
          '(("aunt" "X" "jill"))))

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