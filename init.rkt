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

