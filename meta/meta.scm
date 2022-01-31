;; meta-circular interpreter
;; Scheme within Scheme
(load-option 'format)

(define make-initial-environment
  (lambda ()
    (list
      (list (list '+ +)
            (list '- -)
            (list '* *)
            (list '/ /)
            (list '= =)
            (list 'display display)
            (list 'eq? eq?)
            (list 'null? null?)
            (list 'format format)
            (list 'list list)
            (list 'cons cons)
            (list 'car car)
            (list 'cdr cdr)))))

(define *TOP-ENVIRONMENT* (make-initial-environment))

(define *EXIT-TO-POPL-REPL* #!unspecific)  ;; this will become a continuation

(define popl-error
  (lambda (string)
    (format #t "ERROR: ~A~%" string)
    (*EXIT-TO-POPL-REPL* 'an-error-occurred)))

(define env-value
  (lambda (sym env)
    (let ((pr (assoc sym (car env))))
      (if pr (cadr pr) (popl-error (format #f "Undefined variable ~S" sym))))))

(define popl-bind
  (lambda (symbol value env)
    (let ((bindings (car env)))
      (set-car! env (cons (list symbol value) bindings)))
    symbol))

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

(define function-call
  (lambda (expr env)
    (let* ((all (map (lambda (x) (popl-eval x env)) expr))
           (fn (car all))
           (args (cdr all)))
      (cond ((procedure? fn) (apply fn args))
        (else
          (let ((copy-env (list (car (fourth fn))))
                    (pairs (zip (second fn) args)))
                    (map (lambda (pairs) (popl-bind (first pairs)
                                         (second pairs) copy-env)) pairs)
                    (popl-eval (third fn) copy-env)))))))

(define make-lambda
  (lambda (expr env)
    (if (null? (cddr expr)) (popl-error (format #f "Empty body"))
    (list 'lambda (second expr) (third expr) env))))

(define (unzip lst)
  (apply map list lst))

(define let-to-lambda
  (lambda (expr)
    (let* ((outer-list (second expr))
          (inner-list (list 'lambda (first (unzip outer-list)) (third expr))))
      (cons inner-list (second (unzip outer-list))))))

(define helper
  (lambda (expr)
    (cond ((null? (cdr (second expr)))
            (list 'let (list (car (second expr))) (third expr)))
            (else (list 'let (list (car (second expr)))
              (helper (list 'let (cdr (second expr)) (third expr))))))))

(define let*-to-let
  (lambda (expr)
    (helper expr)))

(define make-cond
  (lambda (expr env)
    (cond ((eq? (caar expr) 'else) (popl-eval (cadr (car expr)) env))
          ((eq? (popl-eval (caar expr) env) #f) (make-cond (cdr expr) env))
          (else (popl-eval (cadar expr) env)))))

(define make-set
  (lambda (expr env)
    (let ((pr (assoc (second expr) (car env))))
      (if pr (popl-bind (second expr) (third expr) env)
             (popl-error (format #f "Unbound variable: ~S" (second expr)))))))

(define popl-eval
  (lambda (expr env)
    (cond ((number? expr) expr)
          ((boolean? expr) expr)
          ((string? expr) expr)
          ((null? expr) expr)
          ((symbol? expr) (env-value expr env))
          ((pair? expr)
            (cond ((eq? (first expr) 'define)
                     (let ((sym (second expr))
                           (val (popl-eval (third expr) env)))
                        (popl-bind sym val env)))
                  ((eq? (first expr) 'if)
                   (if (popl-eval (second expr) env)
                       (popl-eval (third expr) env)
                       (popl-eval (fourth expr) env)))
                  ((eq? (first expr) 'quote) (second expr))
                  ((eq? (first expr) 'lambda) (make-lambda expr env))
                  ((eq? (car expr) 'let) (popl-eval (let-to-lambda expr) env))
                  ((eq? (car expr) 'let*) (popl-eval
                                          (let-to-lambda (let*-to-let expr))
                                                                       env))
                  ((eq? (car expr) 'cond) (make-cond (cdr expr) env))
                  ((eq? (car expr) 'set!) (make-set expr env))
                  (else (function-call expr env))))
          (else 'i-dont-know))))  ;; a placeholder

(define call/cc call-with-current-continuation)

(define popl-repl
  (lambda ()
    (call/cc (lambda (c) (set! *EXIT-TO-POPL-REPL* c)))
    (format #t "H]=> ")
    (let* ((expr (read))
           (value (popl-eval expr *TOP-ENVIRONMENT*)))
       (format #t "~S~%" value))
    (popl-repl)))
