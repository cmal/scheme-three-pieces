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
