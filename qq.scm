;; (qq exps...)
;; 
(define (qq qe env)
  (if (null? qe)
      qe
      (cond ((eq? (car qe) 'qq) (qq (cadr qe) env))
            ((eq? (car qe) 'uq) (list '.eval (cadr qe) env))
            (else (cons (cons '! (car qe)) (qq (cdr qe) env))))))

(qq '() '())
(qq '(a b) '())
(qq '(uq 1) '())
(qq '((uq 1)) '())
(qq '(a) '())
(qq '(a b (uq 1)) '())

(define (qq e env)
  (if (null? e)
      e
      (let ((x (if (eq? (car e) 'uq) (cons '! (car e)) (cons '@ e))))
        (cons x (qq (cdr e) env)))))

(define (geval e env)
  (list e '=> '<val>))

(define (qq e env)
  (if (null? e)
      e
      (cond ((pair? (car e)) (qq (car e) env))
            ((eq? (car e) 'uq)
             ;; (cons (cons '! (car e)) (qq (cdr e) env))
             (cons (geval (cadr e) env) (qq (cdr e) env)))
            (else (cons (car e) (qq (cdr e) env))))))


(qq '(a (uq b) c) 1)
(eq? (car '(uq b)) 'uq)

;; (if (eq? (car '(uq 1)) 'uq) 1 2)
