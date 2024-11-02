;;; predicates



;;; selectors

;;; evaluator

(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence exp env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp) (mapply (meval (operator exp) env)
                                  (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure) 
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure arguments))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (meval (first-operand exps) env)
            (list-of-values (cdr exps) env))))

;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((f (meval (first-operand exps) env))) ;;; l-to-r order ?
;;         (cons f (list-of-values (cdr exps) env)))))

(define (eval-if exp env)
  (if (true? (meval (if-predicate exp) env))
      (meval (if-consequent exp) env)
      (meval (if-alternative exp) env)))

;; (define (eval-if exp env)
;;   (meval (if (true? (meval (if-p exp) env)) (if-t exp) (if-f exp)) env))

;; schema : a b c d
;; match a b c d => ...
;;
;; IF = 'if' cond then else
;; match exp with 'if' cond then else => e (if (e cond r) then else) r
;;           with ...
;;           with ...
;;

;; eval if ec et ef => e[e[ec]r ? et : ef]r
;; [...] <= interpretation de ... dans un domain
;; a,b,c[...]d,e,f <= avec des parametre qui reviennent tout le temps
;;                    ecrit a gauche ou droite pour faire genre (mais ca separe quand
;;                    meme le monde de l'expression et des paremtres

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (meval (first-exp exps) env))
        (else (meval (first-exp exps) env)
              (eval-sequence (rest-exp exps) env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (meval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
   (definition-variable exp)
   (meval (definition-value exp) env)
   env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #true)
        ((string? exp) #true)
        (else #false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp) (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp) (if (symbol? (cadr exp)) (caddr exp) (make-lambda (cdadr exp) (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (if (not (null? (cdddr exp))) (cdddr exp)))

(define (make-if p c a) (list 'if p c a))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((c (car clauses))
            (cs (cdr clauses)))
        (if (cond-else-clause? c)
            (make-begin (cond-actions c))
            (make-if (cond-predicate c)
                  (make-begin (cond-actions c))
                  (expand-clauses cs))))))

;;; (meval (cond->if '(cond ((= 1 1) (display "yo") (display "lo")) ((= 2 2) 2) (else 3) (else 4))) (interaction-environment))
;;;
;;; (cond ... (else ...) (else ...)) is silently accepted, only the first else will be used, the rest is ignored

(define (true? x) (not (eq? x #false)))
(define (false? x) (eq? x #false))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? exp) (tagged-list? exp 'procedure))
(define (procedure-parameters exp) (cadr exp))
(define (procedure-body exp) (caddr exp))
(define (procedure-environment exp) (cadddr exp))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
;; (define (empty-environment '()))
(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  ;; (make-frame (cons var (frame-variables frame))
  ;;             (cons val (frame-values frame)))
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))) ;; ? come on hal... no abstraction layer ?

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Wrong length in " vars vals)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    ;; (newline) (display "env-loop: ") (display (format #f "v: ~s, e: ~s" var env)) (newline)
    (define (scan vars vals)
      ;; (display (format #f "scan vars: ~s, vals: ~s" vars vals))
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment) (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  ;; (display (format #f "lookup-variable-value: ~s" var))
  (env-loop env))

;; scheme@(guile-user)> (lookup-variable-value 'x (cons (make-frame '(x y z) '(1 2 3)) the-empty-environment))
;; $27 = 1
;; scheme@(guile-user)> (lookup-variable-value 'z (cons (make-frame '(x y z) '(1 2 3)) the-empty-environment))
;; $28 = 3

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? env) (env-loop (enclosing-environment env)))
            ((eq? (car vars) var) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment) (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? (car vars) var) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;; wat
;; scheme@(guile-user)> (let ((e (cons (make-frame '(x y z) '(1 2 3)) the-empty-environment)))
;;                        (define-variable! 'a 999 e)
;;                        e)
;; $33 = (((x y z) 1 2 3))

;;; better
;; scheme@(guile-user)> (let ((e (cons (make-frame '(x y z) '(1 2 3)) the-empty-environment)))
;;                        (define-variable! 'a 999 e)
;;                        e)
;; $37 = (((a x y z) 999 1 2 3))

(define primitive-procedures
  (list
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (setup-environment)
  (let ((initial-env (extend-environment
                      (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-environment)))
    (define-variable! 'true #true initial-env)
    (define-variable! 'false #false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;;; io
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (meval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
