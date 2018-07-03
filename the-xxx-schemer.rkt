;; M-x run-racket first
;; eval this will got atom? in repl
(define atom? 
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(pair? '())  ;; #f
;; so ..
(car '()) ;; not valid
(cdr '()) ;; not valid

(null? '())
(null? ()) ;; syntax error
(null? 1)

(eq? 1 2)

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '())
(lat? '(Jack))
(lat? '(J '(s c) e))
(lat? '(b a e))
(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     (else (or (eq? x (car l))
               (member? x (cdr l)))))))

(eq? a a)
(member? 'b '(a b))

(define rember
  (lambda (x l)
    (cond
     ((null? l) '())
     ((eq? x (car l)) (cdr l))
     (else (cons (car l)
                 (rember x (cdr l)))))))

(rember 'a '())
(rember 'a '(a))
(rember 'a '(a b c))
(rember 'a '(c b a d e a))
(rember 'a '(c b a))

(pair? '())

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cond
            ((null? (car l)) (firsts (cdr l)))
            (else (cons (car (car l))
                        (firsts (cdr l)))))))))
(firsts '())
(firsts '(() () ()))
(firsts '((a b) (b c) (c a)))

(define insertR
  (lambda (n o l)
    (cond
     ((null? l) '())
     ((eq? o (car l)) (cons o (cons n (cdr l))))
     (else (cons (car l) (insertR n o (cdr l)))))))

(insertR 'g 'e '(a b c d e f))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new lat))
     (else (cons (car lat)
                 (insertL new old (cdr lat)))))))

(insertL 'g 'e '(a b c d e f))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'g 'e '(a b c d e f))
(subst 'g 'e '())

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or
       (eq? o1 (car lat))
       (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

(subst2 'g 'e 'f '(a b c d e f))
(subst2 'g 'f 'e '(a b c d e f))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a (cdr lat)))))))

(multirember 'a '(c a e b a c))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old
                                (cons new
                                      (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertR new old (cdr lat)))))))

(multiinsertR 'a 'a '(c a b d a c))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new
                                (cons old
                                      (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertL new old (cdr lat)))))))

(multiinsertL 'Z 'a '(c a b d a))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst 'Z 'a '(c a b d a))

(atom? 14)

(add1 1)
(add1 1.1)
(sub1 5)
(sub1 5.1)
(sub1 0)

(zero? 0)
(+ 16 12)

(define my-add
  (lambda (a b)
    (cond
     ((zero? a) b)
     (else (my-add (sub1 a) (add1 b))))))

(my-add 16 12)

(define my-sub
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (sub1 (my-sub a (sub1 b)))))))

(define tup?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((number? (car l)) (tup? (cdr l)))
     (else #f))))

(tup? '())
(tup? '(1 2 3))
(tup? '(()))


(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (+ (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4))

;; natural recursion

(* 1 2)

(define my-*
  (lambda (m n)
    (cond
     ((zero? m) 0)
     (else (+ n (my-* (sub1 m) n))))))

(my-* 1 2)
(my-* 3 5)
(my-* 15 15)
(my-* 12 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2) '(3 4 5))
(tup+ '(1 2 3) '(1 2))

(> 3 4)

(define my->
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (my-> (sub1 n) (sub1 m))))))

(my-> 3 4)
(my-> 0 0)
(my-> 4 3)
(my-> 3 3)

(define my-<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (my-< (sub1 n) (sub1 m))))))

(my-< 3 4)
(my-< 0 0)
(my-< 4 3)
(my-< 3 3)

(define my-=
  (lambda (n m)
    (cond
     ((zero? n) (zero? m))
     ((zero? m) #f)
     (else (my-= (sub1 n) (sub1 m))))))

(my-= 3 4)
(my-= 0 0)
(my-= 4 3)
(my-= 3 3)

(define my-=
  (lambda (n m)
    (cond
     ((my-< n m) #f)
     ((my-> n m) #f)
     (else #t))))

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (* n (^ n (sub1 m)))))))

(^ 2 3)
(^ 0 3)
(^ 3 0)
(^ 3 3)

(define quot
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quot (- n m) m))))))

(quot 5 10)
(quot 10 5)
(quot 7 3)
(quot 99 3)
(quot 10 5)

(length '())

(define my-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (my-length (cdr lat)))))))

(my-length '())
(my-length '(1 2 3))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 1 '(1 2))
(pick 1 '(1))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 1 '(a b c d e))
(rempick 5 '(a b c d e))

(number? 'a)
(number? 1)

(define no-num
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-num (cdr lat)))
     (else (cons (car lat) (no-num (cdr lat)))))))

(no-num '(1 a 2 b 3 c))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(all-nums '(1 a 2 b 3 c 4))

(define atom?
  (lambda (x)
    (and
     (not (list? x))
     (not (null? x)))))

(atom? 'a)
(atom? 1)

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((and (atom? a1) (atom? a2)) (eq? a1 a2))
     (else #f))))

(eqan? 1 1)
(eqan? 1 2)
(eqan? 'a 1)
(eqan? 'a 'b)
(eqan? 'a 'a)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(occur 'a '(1 2 3 a a b c a))

(define one?
  (lambda (x)
    (cond
     ((zero? (sub1 x)) #t)
     (else #f))))

(define one?
  (lambda (x)
    (= x 1)))

;; (define one?
;;   (lambda (x)
;;     (cond
;;      ((number? x) (= x 1))
;;      (else #f))))

(one? 1)
(one? 'a)

(define rempick ;; use one
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (repick (sub1 n) (cdr lat)))))))

(rempick 1 '(1 2))

(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((list? (car lat)) (cons (rember* a (car lat))
                              (rember* a (cdr lat))))
     ((atom? (car lat)) (cond
                         ((eq? a (car lat)) (rember* a (cdr lat)))
                         (else (cons (car lat) (rember* a (cdr lat)))))))))


(rember* 'cup '((coffee) cup ((tea) cup)
                (and (hick)) cup))

(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))


(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define insertR*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((list? (car lat)) (cons (insertR* new old (car lat))
                              (insertR* new old (cdr lat))))
     ((atom? (car lat))
      (cond
       ((eq? old (car lat)) (cons old (cons new (cdr lat))))
       (else (cons (car lat) (insertR* new old (cdr lat)))))))))

(insertR* 'roast 'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

(define occur*
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((list? (car lat)) (+ (occur* a (car lat)) (occur* a (cdr lat))))
     ((atom? (car lat))
      (cond
       ((eq? a (car lat)) (add1 (occur* a (cdr lat))))
       (else (occur* a (cdr lat))))))))

(occur* 'a '())
(occur* 'a '(a a a c))
(occur* 'a '((a b c) (c ((c ((c a)))) ((a)))))

(define subst*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((list? (car lat)) (cons (subst* new old (car lat))
                              (subst* new old (cdr lat))))
     ((atom? (car lat))
      (cond
       ((eq? old (car lat)) (cons new (subst* new old (cdr lat))))
       (else (cons (car lat) (subst* new old (cdr lat)))))))))

(subst* 'z 'a '(a a a))
(subst* 'z 'a '())
(subst* 'z 'a '(((c a)) ((c (((a)) ((c)))))))

(define insertL*
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((list? (car lat)) (cons (insertL* new old (car lat))
                              (insertL* new old (cdr lat))))
     ((atom? (car lat)) ;; else
      (cond
       ((eq? old (car lat)) (cons new (cons old (insertL* new old (cdr lat)))))
       (else (cons (car lat) (insertL* new old (cdr lat)))))))))

(insertL* 'roast 'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

(define member*
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((list? (car lat)) (or (member* a (car lat))
                            (member* a (cdr lat))))
     ((atom? (car lat)) ;; else
      (cond
       ((eq? a (car lat)) #t)
       (else (member* a (cdr lat))))))))

(member* 'a '(a b c))
(member* 'a '())
(member* 'a '((((c a)))))

(define leftmost
  (lambda (lat)
    (cond
     ((atom? (car lat)) (car lat))
     ((list? (car lat)) (leftmost (car lat))))))

(leftmost '()) ;; should error
(leftmost '(((((()))))))
(leftmost '(((a) (s (((c)))))))
(leftmost '(((((a))))))
(or 1 2)
(and 1 2)

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? l1) (atom? l2)) (eqan? l1 l2))
     ((and (list? l1) (list? l2)) (and (eqlist? (car l1) (car l2))
                                       (eqlist? (cdr l1) (cdr l2))))
     (else #f))))


(eqlist? 1 1)
(eqlist? '() '())
(eqlist? '() '(()))
(eqlist? '((((a)))) '((((a)))))
(eqlist? '((((a)))) '((((a) b))))

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((eqlist? s (car l)) (rember s (cdr l)))
     (else (cons (car l) (rember s (cdr l)))))))

(rember '(a) '((a) b c d))
(rember '(a) '(() (a) (a b)))


(define equal?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? l1) (atom? l2)) (eq? l1 l2))
     ((and (list? l1) (list? l2)) (and (equal? (car l1) (car l2))
                                       (equal? (cdr l1) (cdr l2))))
     (else #f))))

(equal? '() '())
(equal? '() 'a)
(equal? 'a '())
(equal? '() '(a))
(equal? '(a) '())
(equal? '(a) '(a))
(equal? 'a 'a)
(equal? '(a b) '(a))
(equal? '(a) '(a b))
(equal? 'a '(a))
(equal? '(a) 'a)
(equal? '(a (b c)) '(a (b c)))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

(set? '())
(set? '(1))
(set? '(1 2 (1 2 ())))
(set? '(1 2 3 1))
(set? '((1 2)))

(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     (else (or (equal? x (car l))
               (member? x (cdr l)))))))


(define makeset
  (lambda (l)
    (cond
     ((null? l) l)
     ((member? (car l) (cdr l)) (makeset (cdr l)))
     (else (cons (car l) (makeset (cdr l)))))))

(makeset '())
(makeset '(1))
(makeset '(1 2 1))
(makeset '((1) (2) (1)))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2) (subset? (cdr set1) set2))
     (else #f))))

(subset? '() '(1 2))
(subset? '(2) '(1 2))
(subset? '(1 2) '(1 2))
(subset? '(1 2 3) '(1 2))
(subset? '(3) '(1 2))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '() '())
(eqset? '(() (1)) '((1) ()))
(eqset? '(1 2) '(2 1 3))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else (intersect? (cdr set1) set2)))))

(intersect? '(1) '(2))
(intersect? '(1) '(2 1))
(intersect? '(1 2) '(3 1))
(intersect? '(1) '())
(intersect? '() '(1))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '() '(1 2 3))
(intersect '(1 2 3) '())
(intersect '(2) '(1 2 3))
(intersect '(1 2 3) '(2))
(intersect '(1 2 3) '(4))
(intersect '(1 2 3) '(2 3 4))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(union '() '(1 2 3))
(union '(1 2 3) '())
(union '(2) '(1 2 3))
(union '(1 2 3) '(2))
(union '(1 2 3) '(4))
(union '(1 2 3) '(2 3 4))

(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (difference (cdr set1) set2))
     (else (cons (car set1) (difference (cdr set1) set2))))))

(difference '(1 2 3) '(2 3 4))

(define intersectall
  (lambda (lset)
    (cond
     ((null? lset) '())
     ((null? (cdr lset)) (car lset))
     (else (intersectall (cons (intersect (car lset)
                                         (car (cdr lset)))
                              (cdr (cdr lset))))))))

(intersectall '((1 2 3) (2 3 4)))
(intersectall '((1 2 3) (2 3 4) (3 4 5)))

(define a-pair?
  (lambda (l)
    (cond
     ((null? l) #f)
     ((atom? l) #f)
     ((null? (cdr l)) #f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(a-pair? '())
(a-pair? '(1))
(a-pair? 1)
(a-pair? '(1 2))
(a-pair? '(1 2 3))

(define my-first
  (lambda (l)
    (car l)))

;; (my-first '())
(my-first '(1 2))

(define my-second
  (lambda (l)
    (car (cdr l))))

(my-second '(1 2))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(build 1 2)

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define pairs?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((pair? (car l)) (pairs? (cdr l)))
     (else #f))))

(pairs? '((1 2) () (3 4)))
(pairs? '((1 2) (5 7) (3 4)))

(define rel?
  (lambda (l)
    (and (set? l) (pairs? l))))

(rel? '(apples peaches pumpkin pie))
(rel? '((apples peaches) (pumpkin pie) (apples peaches)))
(rel? '((apples peaches) (pumpkin pie)))
(rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))
(firsts '((4 3) (4 2) (7 6) (6 2) (3 4)))

(define fun?
  (lambda (l)
    (set? (firsts l))))

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build
                  (car (cdr (car rel)))
                  (car (car rel)))
                 (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define revpair
  (lambda (pair)
    (build (car (cdr pair)) (car pair))))

(revpair '(1 2))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr (car l)))
                 (seconds (cdr l)))))))

(seconds '())
(seconds '((1 2)))
(seconds '((8 3) (4 2) (7 6) (6 2) (3 4)))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fullfun? '((8 3) (4 5) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin) (plum prune) (stewed prune)))
(fullfun? '((grape raisin) (plum prune) (stewed grape)))

(define one-to-one? ;; the same as fullfun?
  (lambda (rel)
    (fun? (revrel rel))))

(one-to-one? '((chocolate chip) (doughy cookie)))

(rember 'a '(b c a))
(rember 'c '(b c a))

(define rember-f
  (λ (pred a l)
    (cond
      ((null? l) '())
      ((pred a (car l)) (cdr l))
      (else (cons (car l)
                  (rember-f pred a (cdr l)))))))

(rember-f eq? 'a '(b c a))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (λ (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))

(eq?-salad 'salad)
(eq?-salad 'corn)

(define rember-f
  (lambda (test?)
    (λ (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

((rember-f eq?) 'a '(a b c))
((rember-f eq?) 'a '(c b a))
;; (rember-f eq? 'a '(a b c))

(define rember-eq?
  (rember-f eq?))

(rember-eq? 'a '(a b c))

(define insertL-f
  (λ (test?)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new (cons old ((insertL-f test?) new old (cdr l)))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

((insertL-f eq?) 'z 'a '(a c b a d e a))

(define insertR-f
  (lambda (test?)
    (λ (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons old (cons new ((insertR-f test?) new old (cdr l)))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

((insertR-f eq?) 'z 'a '(a c b a d e a))

(define seqL
  (λ (a b c)
    (cons a (cons b c))))

(seqL 'a 'b '(c))

(define seqR
  (lambda (a b c)
    (cons b (cons a c))))

(seqR 'a 'b '(c))

(define insert-g
  (λ (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seq new old ((insert-g seq) new old (cdr l))))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(insertL 'z 'a '(a c b a d e a))
(insertR 'z 'a '(a c b a d e a))

(define seqS
  (lambda (new old l)
    (cons new l)))

(seqS 'a 'b '(c))
(trace subst)
(trace insert-g)
(trace seqS)
(define subst
  (insert-g seqS))

(lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seqS new old (recur new old (cdr l))))
        (else (cons (car l) (recur new old (cdr l))))))

(subst 'g 'e '(a b c d e f))
(subst 'g 'e '())


(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(define seqrem
  (λ (new old l)
    l))

(yyy 'sausage '(pizza with sausage and bacon))

(define atom-to-function
  (λ (atom)
    atom))

(define operator
  (λ (nexp)
    (car nexp)))

(atom-to-function (operator '(+ 5 3)))


(define value
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (car nexp))
             (value (car (cdr car))))))))

(value '(+ (+ 3 5) (* 2 1)))

(define multirember
  (λ (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(multirember 'a '(a b c z a))

(define multirember-f
  (lambda (test?)
    (λ (a lat)
     (cond
       ((null? lat) '())
       ((test? (car lat) a)
        (multirember a (cdr lat)))
       (else (cons (car lat)
                   (multirember a (cdr lat))))))))

((multirember-f eq?) 'a '(a b c z a))

(define multirember-eq?
  (multirember-f eq?))

(multirember-eq? 'a '(a b c z a))

(define multiremberT
  (λ (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(define eq?-a (eq?-c 'a))

(eq?-a 'a)

(multiremberT eq?-a '(a b c a c z))

(define multirember&co
  (λ (a lat col) ;; col stands for collector or continuation
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a) (multirember&co a (cdr lat)
                                         (lambda (newlat seen)
                                           (col newlat
                                                (cons (car lat) seen)))))
      (else (multirember&co a
                            (cdr lat)
                            (λ (newlat seen)
                              (col (cons (car lat) newlat)
                                   seen)))))))

(define a-friend
  (λ (x y)
    (null? y)))

(a-friend 'a 'b)
(a-friend 'a '())

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)

(define new-friend
  (λ (newlat seen)
    (a-friend newlat
              (cons (car '(tuna)) seen))))

(new-friend '() '())

(multirember&co 'tuna '(and tuna) a-friend)

;; ->
(multirember&co 'tuna '(tuna) new-friend)
(define new-friend
  (λ (newlat seen)
    (a-friend (cons 'and newlat) seen)))

;; ->
(multirember&co 'tuna '() latest-friend)
;; lat: '(tuna)
;; col: new-friend
(define latest-friend
  (λ (newlat seen)
    (new-friend newlat (cons 'tuna seen))))

;; ->
(latest-friend '() '())

;; ->
(new-friend '() '(tuna))

;; ->
(a-friend '(and) '(tuna))

;; ->
(null? '(tuna))


(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '(strawberries and swordfish) a-friend)

(multirember&co 'tuna '(strawberries tuna and swordfish) (λ (x y) (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))

(multirember 'a '(a b c z a))
(multirember&co 'a '(a b c z a) (lambda (x y) x))


(multiinsertLR 'z 'a 'b '(a b c d e f g b a))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0)) ;; newlat left-insertions right insertions
      ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat nleft nright)
                                                (col (cons new (cons oldL newlat))
                                                     (add1 nleft)
                                                     nright))))
      ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
                                              (λ (newlat nleft nright)
                                                (col (cons oldR (cons new newlat))
                                                     nleft
                                                     (add1 nright)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat)
                              (λ (newlat nleft nright)
                                (col (cons (car lat) newlat)
                                     nleft
                                     nright)))))))

(multiinsertLR&co 'z 'a 'b '(a b c d e f g b a) (λ (x y z) z))

(multiinsertLR&co
 'cranberries
 'fish
 'chips
 '()
 (λ (x y z) z))


(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips)
                  (λ (x y z) z))

(define even?
  (λ (n)
    (= (* (quot n 2) 2) n)))

(even? 2)
(even? 0)
(even? 1)
(even? 642)
(even? 6421)

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                         (else (evens-only* (cdr l)))))
      ((list? (car l)) (cons (evens-only* (car l))
                             (evens-only* (cdr l)))))))

(evens-only*
 '(() (((1 3) (2 4)) (3 (4 3 9 6 5 (5 (6 (((4) (7 3 4))))))))))
(evens-only*
 '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (λ (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l)) (cond
                         ((even? (car l)) (evens-only*&co (cdr l)
                                                          (lambda (newlat prod sum)
                                                            (col
                                                             (cons (car l) newlat)
                                                             (* (car l) prod)
                                                             sum))))
                         (else (evens-only*&co (cdr l)
                                               (lambda (newlat prod sum)
                                                 (col
                                                  newlat
                                                  prod
                                                  (+ (car l) sum)))))))
      ((list? (car l)) (evens-only*&co (car l)
                                       (λ (newlat prod sum)
                                         (evens-only*&co (cdr l)
                                                         (λ (newlat1 prod1 sum1)
                                                           (col
                                                            (cons newlat newlat1)
                                                            (* prod prod1)
                                                            (+ sum sum1))))))))))

(define the-last-friend
  (λ (newl product sum)
    (cons sum
          (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

(evens-only*&co '(9 1 2 8)
                (λ (newlat prod sum)
                  (evens-only*&co '(3 10 ((9 9) 7 6) 2)
                                  (λ (newlat1 prod1 sum1)
                                    (cons
                                     (+ sum sum1)
                                     (cons (* prod prod1)
                                           (cons newlat newlat1)))))))

;; l is '(9 1 2 8)
;; col is
(λ (newlat prod sum)
  (evens-only*&co '(3 10 ((9 9) 7 6) 2)
                  (λ (newlat1 prod1 sum1)
                    (cons
                     (+ sum sum1)
                     (cons (* prod prod1)
                           (cons newlat newlat1))))))

(col
 newlat
 prod
 (+ 9 sum))

(evens-only*&co '(1 2 8)
                (lambda (newlat prod sum)
                  (evens-only*&co '(3 10 ((9 9) 7 6) 2)
                                  (λ (newlat1 prod1 sum1)
                                    (cons
                                     (+ (+ 9 sum) sum1)
                                     (cons (* prod prod1)
                                           (cons newlat newlat1)))))))

;; 因为同构，推算一下

(evens-only*&co '()
                (lambda (newlat prod sum)
                  (evens-only*&co '(3 10 ((9 9) 7 6) 2)
                                  (λ (newlat1 prod1 sum1)
                                    (cons
                                     (+ (+ 1 (+ 9 sum)) sum1)
                                     (cons
                                      (* (* 8 (* 2 prod)) prod1)
                                      (cons newlat newlat1)))))))

(evens-only*&co '(3 10 ((9 9) 7 6) 2)
                (λ (newlat1 prod1 sum1)
                  (cons
                   (+ 10 sum1)
                   (cons
                    (* 16 prod1)
                    (cons '() newlat1)))))

caviar


(define looking
  (λ (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a b lat)
    (cond
      ((number? (pick b lat)) (keep-looking a (pick b lat) lat))
      ((atom? (pick b lat)) (eq? a (pick b lat)))
      (else #f))))
;; unnatural recursion

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))

(define second my-second)
(define shift
  (lambda (pair)
    (cond
      ((null? pair) '())
      (else (build (first (first pair))
                   (build (second (first pair))
                          (second pair)))))))

(define first my-first)
(shift '((a b) c))
(shift '((a b) (c d)))

(define align
  (λ (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(align '(a b))
(align '((a b) c))
(a-pair? (first '((a b) c)))
(shift '((a b) c))

(align '((a b) (c d)))
(align '(() (c d)))
(align '(a b c d))
(align 'a)
;; (align '())

(define eternity
  (λ (x)
    (eternity x)))

;; (eternity 'x)


(define e1
  (λ (x)
    (e2 x)))

(define e2
  (lambda (x)
    (e1 x)))

(e1 'x)

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

(length* '((a b) (c d)))

(define numbered?
  (λ (aexp)
    (cond
      ((atom? (car aexp)) (number? aexp))
      (else (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(define weight*
  (λ (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))))))

(weight* '((a b) c))
(weight* '(a (b c)))

(define shuffle
  (λ (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '(a b))
(shuffle '((a b) (c d)))
(revpair '((a b) (c d)))

(define C
  (λ (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (quot n 2)))
         (else (C (add1 (* 3 n)))))))))

(C 0)
(C 12343417)

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(A 1 0)
(A 1 1)
(A 2 2)
(A 4 3)
(A 3 (A 4 2))
(A 3 (A 3 (A 4 1)))
(A 3 (A 3 (A 4 (A 3 (A 2 (A 1 (A 1 (A 2 (A 1 (A 2 0))))))))))


(require racket/trace)

(trace A)

(define last-try
  (λ (x)
    (and (will-stop? last-try)
         (eternity x))))

(last-try 1)

;; can describe, but cannot define
;; (define will-stop? (λ (l) ...))

(λ (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))


(((lambda (mk-length)
          (mk-length mk-length))
  (λ (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1
               ((mk-length eternity)
                (cdr l))))))))
 '(apples))

((lambda (l)
   (cond
     ((null? l) 0)
     (else (add1
            ((eternity eternity)
             (cdr l)))))) '())



(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     (mk-length mk-length))))
 '(apples))


(((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     (mk-length mk-length)))
   (lambda (mk-length)
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     (mk-length mk-length)))))
 '(apples))


(define Y
  (λ (le)
    ((lambda (f) (f f))
     (λ (f)
       (le (λ (x) ((f f) x)))))))

(define new-entry build)

(define lookup-in-entry
  (λ (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (λ (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((equal? name (car names)) (car values))
      (else (lookup-in-entry-help
             name (cdr names) (cdr values) entry-f)))))

(lookup-in-entry 'entree
                 '((appetizer entree beverage)
                   (food tastes good))
                 (λ (x) x))

(lookup-in-entry '(pate boeuf vin)
                 '(((appetizer entree beverage) (pate boeuf vin))
                   ((beverage dessert) ((food is) (number one with us))))
                 (λ (x) x))

(lookup-in-entry '(pate boeuf vin)
                 '(((appetizer entree beverage) (pate boeuf vin))
                   ((beverage dessert) ((food is) (number one with us))))
                 (λ (x) x))

(define extend-table cons)

(define lookup-in-table
  (λ (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(lookup-in-table
  'entree
  '(((appetizer entree beverage) (pate boeuf vin))
    ((beverage dessert) ((food is) (number one with us))))
  (λ (x) x))

(lookup-in-table
  'entree
  '(((entree dessert) (spaghetti spumoni))
    ((appetizer entree beverage) (food tastes good)))
  (λ (x) x))

(car (quote (a b c)))

(value (car (quote (a b c))))
(value (quote (car (quote (a b c))))) ;; ???

(value '(add1 6))

((λ (nothing)
   (cons nothing '()))
 '(from nothing comes something))

(cons '(from nothing comes something) '())

((lambda (nothing)
   (cond
     (nothing 'something)
     (else 'nothing)))
 #t)

;; (type 6)

(value car)


;; 6 types:
;; *const
;; *quote
;; *identifier
;; *lambda
;; *cond
;; *application.

(define atom-to-action
  (λ (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (λ (e)
    (cond
      ((atom? (car e)) (cond
                         ((eq? (car e) 'quote) *quote)
                         ((eq? (car e) 'lambda) *lambda)
                         ((eq? (car e) 'cond) *cond)
                         (else *application)))
      (else *application))))

(define expression-to-action
  (λ (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (λ (e table)
    ((expression-to-action e) e table)))

(define value
  (λ (e)
    (meaning e '())))

(define *const
  (λ (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (λ (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (λ (name)
    (car '())))

(define *lambda
  (λ (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define else?
  (λ (x)
    (cond
      ((atom? x) eq? x 'else)
      (else #f))))

(define question-of first)
(define answer-of second)

(define evcon
  (λ (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       table)
      (else (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (λ (e table)
    (evcon (cond-lines-of e) table)))

(*cond
 '(cond
    (coffee party)
    (else klatsch))
 '(((coffee) (#t))
   ((klatsch party) (5 (6)))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (λ (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (λ (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (λ (l)
    (eq? (first l) (quote non-primitive))))

(define apply
  (λ (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define :atom?
  (λ (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive?) #t)
      (else #f))))

(define apply-primitive
  (λ (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals))))))

(define apply-closure
  (λ (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

(apply-closure
 '((((u v w)
     (1 2 3))
    ((x y z)
     (4 5 6)))
   (x y)
   (cons z x))
 '((a b c) (d e f)))





(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))

(define plus
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          (m (f ((n f) x))))))))
