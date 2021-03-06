(define-macro when
  (lambda (test . branch)
    (list 'if test
          (cons 'begin branch))))

(define-macro my-or
  (lambda (x y)
    `(if ,x ,x ,y)))

(define-macro fluid-let
  (lambda (xexe . body)
    (let ((xx (map car xexe))
          (ee (map cadr xexe))
          (old-xx (map (lambda (ig) (gensym)) xexe))
          (result (gensym)))
      `(let ,(map (lambda (old-x x)
                    `(,old-x ,x))
                  old-xx xx)
         ,@(map (lambda (x e)
                  `(set! ,x ,e))
                xx ee)
         (let ((,result (begin ,@body)))
           ,@(map (lambda (x old-x)
                    `(set! ,x ,old-x))
                  xx old-xx)
           ,result)))))
