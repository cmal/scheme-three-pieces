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
