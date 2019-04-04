
(define list-product
  (lambda (s)
    (let recur ((s s))
      (if (null? s) 1
          (* (car s) (recur (cdr s)))))))


(define list-product
  (lambda (s)
    (call/cc
      (lambda (exit)
        (let recur ((s s))
          (if (null? s) 1
              (if (= (car s) 0) (exit 0)
                  (* (car s) (recur (cdr s))))))))))


(+ (* 3 4) (* 5 6))

((lambda (k)
   (k* 3 4 (lambda (v34)
             (k* 5 6 (lambda (v56)
                       (k+ v34 v56 k))))))
 halt)

(define (kcall/cc kfn k)
  (kfn (lambda (value ignored-continuation)
         (k value))
       k))

(+ (* 3 4) (call/cc (lambda (exit) (* 5 (exit 6)))))

((lambda (k)
  (k* 3 4 (lambda (v34)
            (kcall/cc (lambda (exit k)
                        (exit 6 (lambda (v6)
                                 (k* 5 v6 k)))
                      k)))))
 halt)
