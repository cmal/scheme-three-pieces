For the list form Steele defines three operators, map, reduce, and mapreduce. They are:
— Steele map —
(define (map f xs) ; linear in (length xs)
  (cond ((null? xs) ’())
        (else (cons (f (car xs))
                    (map f (cdr xs))))))
———-
so, for example,
(map (λ (x) (* x x)) ’(1 2 3)) ⇒ (1 4 9)
— Steele reduce —
(define (reduce g id xs) ; linear in (length xs)
  (cond ((null? xs) id)
        (else (g (car xs) (reduce g id (cdr xs))))))
———-
so, for example,

(reduce + 0 ’(1 4 9)) ⇒ 14
— Steele mapreduce —
(define (mapreduce f g id xs) ; linear in (length xs)
  (cond ((null? xs) id)
        (else (g (f (car xs)) (mapreduce f g id (cdr xs))))))
———-
so, for example,
(mapreduce (λ (x) (* x x)) + 0 ’(1 2 3)) Rightarrow 14
— Steele length —
(define (length xs) ; linear in (length xs)
  (mapreduce (lambda (q) 1) + 0 xs))
———-
Using structural recursion, we can define filter as:
— Steele filter —
(define (filter p xs) ; linear in (length xs)
  (cond ((null? xs) ’())
        ((p (car xs)) (cons p (filter p (cdr xs))))
        (else (filter p (cdr x)))))
———-
Alternatively we could map the predicate down the list:
— Steele filter 2 —
(define (filter p xs) ; linear in (length xs)??
  (apply append
         (map (lambda (x) (if (p x) (list x) ’())) xs)))
———-
Or, by using mapreduce
— Steele filter 3 —
(define (filter p xs) ; linear in (length xs)!!
  (mapreduce (lambda (x) (if (p x) (list x) ’()))
             append ’() xs)) ; each call to append is constant time
———-
The reverse function could be defined structurally as:
— Steele reverse 1 —
(define (reverse xs) ; quadratic in (length xs)
  (cond ((null? xs) ’())
        (else (addright (reverse (cdr xs)) (car xs)))))
———-
Or we could define a new function, revappend, by using the accumulator trick to reduce the time complexity from quadratic to linear.
— Steele revappend —
(define (revappend xs ys) ; linear in (length xs)
  (cond ((null? xs) ys)
        (else (revappend (cdr xs) (cons (car xs) ys)))))
———-
and use it to define reverse:
— Steele reverse 2 —
(define (reverse xs)
  (revappend xs ’()))
———-
