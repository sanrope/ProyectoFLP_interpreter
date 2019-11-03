#lang eopl

;******************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program ((list Classes)exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}(,)) = <expression>} in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************

;******************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "_" "-" "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (str
   ("|" (arbno (or letter digit)) "|") string)
  ))

;EspecificaciÃ³n SintÃ¡ctica (gramÃ¡tica)

(define grammar-simple-interpreter
  '((program ((arbno class-decl) expression) a-program)
    
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (str) string-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    
    ; caracterÃsticas adicionales
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                set-exp)

    ;;;;;;
    ;; PRIMITIVAS BASICAS
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    ;; PRIMITIVAS BOOLEANAS
    (primitive ("==") equal-prim)
    (primitive ("=or") equalor-prim)
    (primitive ("=&") equaland-prim)
    (primitive (">") morethan-prim)
    (primitive (">&") morethanand-prim)
    (primitive (">or") morethanor-prim)
    (primitive ("<") lessthan-prim)
    (primitive ("<&") lessthanand-prim)
    (primitive ("<or") lessthanor-prim)
    (primitive (">=") morethanequal-prim)
    (primitive (">=&") morethanequaland-prim)
    (primitive (">=or") morethanequalor-prim)
    (primitive ("<=") lessthanequal-prim)
    (primitive ("<=&") lessthanequaland-prim)
    (primitive ("<=or") lessthanequalor-prim)
    (primitive ("max") max-prim)
    (primitive ("min") min-prim)
    (primitive ("&") and-prim)
    (primitive ("&&") andand-prim)
    (primitive ("or") or-prim)
    (primitive ("or|") oror-prim)
    ;; PRIMITIVAS CON LISTAS
    ;(primitive ("car") car-prim)
    ;(primitive ("cdr") cdr-prim)
    ;(primitive ("cons") cons-prim)
    ;(primitive ("null?") null?-prim)


    ;************DECL para objetos*******************

     (class-decl                         
      ("class" identifier  
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("method" identifier 
        "("  (separated-list identifier ",") ")" ; method ids
        expression 
        )
      a-method-decl)

    (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)

    ;(expression                                
     ; ("super" identifier    "("  (separated-list expression ",") ")")
      ;super-call-exp)
    
            ))


;Tipos de datos para la sintaxis abstracta de la gramÃ¡tica

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?)))
;  (letrec-exp
;   (proc-names (list-of symbol?))
;   (idss (list-of (list-of symbol?)))
;   (bodies (list-of expression?))
;   (body-letrec expression?))
;  (begin-exp
;   (exp expression?)
;   (exps (list-of expression?)))
;  (set-exp
;   (id symbol?)
;   (rhs expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automÃ¡ticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************
;Parser, Scanner, Interfaz

;El FrontEnd (AnÃ¡lisis lÃ©xico (scanner) y sintÃ¡ctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador LÃ©xico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + EvaluaciÃ³n + seÃ±al para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************
;El Interprete

;eval-program: <programa> -> numero
; funciÃ³n que evalÃºa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
                 (elaborate-class-decls! c-decls)
                 (eval-expression exp (empty-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresiÃ³n en el ambiente de entrada

;******************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;******************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (string-exp (str) str)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-let-exp-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))

      (set-exp (id rhs-exp)
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1)
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))


      ;*******EXP objetos*************


      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      
      ;(super-call-exp (method-name rands)
       ; (let ((args (eval-rands rands env))
        ;      (obj (apply-env env 'self)))
         ; (find-method-and-apply
          ;  method-name (apply-env env '%super) obj args)))

      
      
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (exps env)
    (map (lambda (exp) (eval-expression exp env)) exps)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ;; ARITMETHIC PRIMS
      (add-prim () (aux-add args))
      (substract-prim () (aux-substract args))
      (mult-prim () (aux-mult args))
      (div-prim () (aux-div args))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      ; BOOLEAN PRIMS
      (and-prim () ((if (null? args)
                        (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                               (and-aux (args)))))
      (andand-prim () ((if (null? args)
                           (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                           (andand-aux ((car args) (cdr args))))))
      (or-prim () ((if (null? args)
                       (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                       (or-aux ((car args) (cdr args))))))
      (oror-prim () ((if (null? args)
                         (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                         (oror-aux ((car args) (cdr args))))))
      (equal-prim () ((if (null? args)
                          (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                          (equal-aux(args)))))
      (equaland-prim () ((if (null? args)
                             (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                               (equaland-aux(args)))))
      (equalor-prim () ((if (null? args)
                            (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                            (equalor-aux(args)))))
      (morethan-prim () (if (null? args)
                            (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                            (morethan(args))))
      (morethanand-prim () ((if (null? args)
                               (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                               (morethanand (car args) (cdr args)))))
      (morethanor-prim () ((if (null? args)
                              (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                              (morethanor (car args) (cdr args)))))
      (lessthan-prim () ((if (null? args)
                            (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                            (lessthan (args)))))
      (lessthanand-prim () ((if (null? args)
                               (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                               (lessthanand (car args) (cdr args)))))
      (lessthanor-prim () ((if (null? args)
                              (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                              (lessthanor (car args) (cdr args)))))
      (morethanequal-prim () ((if (null? args)
                                  (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                  (morethanequal(args)))))
      (morethanequaland-prim () ((if (null? args)
                                    (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                    (morethanandequal (car args) (cdr args)))))
      (morethanequalor-prim () ((if (null? args)
                                    (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                    (morethanorequal (car args) (cdr args)))))
      (lessthanequal-prim () ((if (null? args)
                                  (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                  (lessthanequal (args)))))
      (lessthanequaland-prim () ((if (null? args)
                                     (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                     (lessthanandequal (car args) (cdr args)))))
      (lessthanequalor-prim () ((if (null? args)
                                    (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                    (lessthanorequal (car args) (cdr args)))))
      (max-prim () ((if (null? args)
                                    (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                    (max-aux (car args) (cdr args)))))
      (min-prim () ((if (null? args)
                                    (eopl:error 'apply-primitive "Attempt to apply procedure with no arguments ~s" args)
                                    (min-aux (car args) (cdr args)))))
      )))
; AUXILIARES +,-,*,/
(define aux-add
  (lambda (args)
    (if (not (null? args))
    (+ (car args) (aux-add (cdr args)))
    0
    )
  )
)
(define aux-substract
  (lambda (args)
    (if (not (null? args))
    (- (car args) (aux-add (cdr args)))
    0
    )
  )
)
(define aux-mult
  (lambda (args)
    (if (not (null? args))
    (* (car args) (aux-add (cdr args)))
    1
    )
  )
)
(define aux-div
  (lambda (args)
    (if (not (null? args))
    (/ (car args) (aux-add (cdr args)))
    1
    )
  )
)

; AUXILIARES AND Y OR
(define and-aux
  (lambda (args)
    (if (not (null? args))
        (if (and (car args) (cadr args))
            (and-aux (cdr args))
            #f
            )
        #t
        ) 
    )
  )

(define andand-aux
  (lambda (initvalue args)
    (if (not (null? args))
        (if (and initvalue (car args))
            (and-aux initvalue (cdr args))
            #f
            )
        #t
        ) 
    )
  )

(define or-aux
  (lambda (args)
    (if (not (null? args))
        (if (or  (car args) (cadr args))
            #t
            (or-aux (cdr args))
            )
        #f
        )
    )
  )

(define oror-aux
  (lambda (initvalue args)
    (if (not (null? args))
        (if (or  initvalue (car args))
            #t
            (oror-aux initvalue (cdr args))
            )
        #f
        )
    )
  )

; AUXILIARES DE MAX Y MIN
(define max-aux
  (lambda (initvalue args)
    (if (not(null? args))
        (if (> initvalue (car args))
            (max-aux initvalue (cdr args))
            (max-aux (car args) (cdr args))
            )
        initvalue
        )
    )
  )

(define min-aux
  (lambda (initvalue args)
    (if (not(null? args))
        (if (< initvalue (car args))
            (min-aux initvalue (cdr args))
            (min-aux (car args) (cdr args))
            )
        initvalue
        )
    )
  )
; AUXILIARES DE MORETHAN Y LESSTHAN
(define morethan
  (lambda (args)
    (if (not(null? args))
        (if (> (car args) (cadr args))
            (morethan (cdr args))
            #f
            )
        #t
        )
    )
  )

(define morethanand
  (lambda (initvalue args)
    (if (not(null? args))
        (if (> initvalue (car args))
            (morethanand initvalue (cdr args))
            #f
            )
        #t
        )
    )
)

(define morethanor
  (lambda (initvalue args)
    (if (not(null? args))
        (if (> initvalue (car args))
            #t
            (morethanor initvalue (cdr args))
            )
        #f
        )
    )
  )

(define lessthan
  (lambda (args)
    (if (not(null? args))
        (if (< (car args) (cadr args))
            (morethan (cdr args))
            #f
            )
        #t
        )
    )
  )

(define lessthanand
  (lambda (initvalue args)
    (if (not(null? args))
        (if (< initvalue (cadr args))
            (morethanand initvalue (cdr args))
            #f
            )
        #t
        )
    )
)

(define lessthanor
  (lambda (initvalue args)
    (if (not(null? args))
        (if (< initvalue (cadr args))
            #t
            (morethanand initvalue (cdr args))
            )
        #f
        )
    )
)

(define morethanequal
  (lambda (args)
    (if (not(null? args))
        (if (>= (car args) (cadr args))
            (morethan (cdr args))
            #f
            )
        #t
        )
    )
  )

(define morethanandequal
  (lambda (initvalue args)
    (if (not(null? args))
        (if (>= initvalue (car args))
            (morethanand initvalue (cdr args))
            #f
            )
        #t
        )
    )
)

(define morethanorequal
  (lambda (initvalue args)
    (if (not(null? args))
        (if (>= initvalue (car args))
            #t
            (morethanor initvalue (cdr args))
            )
        #f
        )
    )
  )

(define lessthanequal
  (lambda (args)
    (if (not(null? args))
        (if (< (car args) (cadr args))
            (morethan (cdr args))
            #f
            )
        #t
        )
    )
  )

(define lessthanandequal
  (lambda (initvalue args)
    (if (not(null? args))
        (if (<= initvalue (car args))
            (morethanand initvalue (cdr args))
            #f
            )
        #t
        )
    )
)

(define lessthanorequal
  (lambda (initvalue args)
    (if (not(null? args))
        (if (<= initvalue (car args))
            #t
            (morethanor initvalue (cdr args))
            )
        #f
        )
    )
  )
; AUXILIARES DE EQUAL
(define equal-aux
  (lambda (args)
    (if (not (null? args))
        (if (equal? (car args)(cadr args))
            (equal-aux (cdr args))
            #f
            )
        #t
        )
    )
  )

(define equaland-aux
  (lambda (initvalue args)
    (if (not(null? args))
        (if (equal? initvalue (car args))
            (equaland-aux initvalue (cdr args))
            #f
            )
        #t
        )
    )
  )

(define equalor-aux
  (lambda (initvalue args)
    (if (not(null? args))
        (if (equal? initvalue (car args))
            (equal-aux initvalue (cdr args))
            #f
            )
        #t
        )
    )
  )


 ;************Define para object****************

 (define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (make-first-part c-decl)
        ;(cons
         ; (make-first-part c-decl)
          ;(new-object (class-decl->super-name c-decl))) ;dudas

        ))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl))))))



;BOOLEAN DATATYPE
(define-datatype boolean boolean?
  (true-value
   (value true-value?))
  (false-value
   (value (not(true-value?))))
  )

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (equal? x #t)))

;*******************************


;*******Definicion de declaraciones*************


 (define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name field-ids m-decls)
        class-name))))

;(define class-decl->super-name
 ; (lambda (c-decl)
  ;  (cases class-decl c-decl
   ;   (a-class-decl (class-name super-name field-ids m-decls)
    ;    super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))




;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))
;************************************
;Methods (objetos)**************

 (define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            ;;(class-name->super-name host-name) 
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name parts) class-name)
      parts
      (view-object-as (parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          ;(super-name (class-name->super-name host-name))
          )
      (eval-expression body
        (extend-env
          ;(cons '%super (cons 'self ids))
          ;(cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids parts)
        (part->fields    parts)
        (build-field-env parts)))))
 




;*******************************
;Ambientes

;definiciÃ³n del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;funciÃ³n que crea un ambiente vacÃo
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacÃo 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;funciÃ³n que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;funciÃ³n que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;********Ambientes de Method*************

 (define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

;*****Ambientes de Class********************

 (define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;**************extend-env-refs****************

(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))


(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))


;****************************************




;iota: number -> list
;funciÃ³n que retorna una lista de los nÃºmeros desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;funciÃ³n que busca un sÃmbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;*******************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (part? x) (string? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))


(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (let
       ((ref (cases target (primitive-deref ref)
               (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval?)))
  ))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;******************************
;****Partes del objeto***************

 (define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

;(define part->super-name
 ; (lambda (part)
  ;  (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

;(define class-name->super-name
 ; (lambda (class-name)
  ;  (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (parts))))


;************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posiciÃ³n de un sÃmbolo
; en la lista de sÃmbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************
;;Pruebas
;
;(show-the-datatypes)
;just-scan
;scan&parse
;(just-scan "add1(x)")
;(just-scan "add1(   x   )%cccc")
;(just-scan "add1(  +(5, x)   )%cccc")
;(just-scan "add1(  +(5, %ccccc x) ")
;(scan&parse "add1(x)")
;(scan&parse "add1(   x   )%cccc")
;(scan&parse "add1(  +(5, x)   )%cccc")
;(scan&parse "add1(  +(5, %ccccx)) ")
;(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
;(scan&parse "let
;x = -(y,1)
;in
;let
;x = +(x,2)
;in
;add1(x)")
;
;(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
;(define exp-numero (lit-exp 8))
;(define exp-ident (var-exp 'c))
;(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
;(define programa (a-program empty exp-app))
;(define una-expresion-dificil (primapp-exp (mult-prim)
;                                           (list (primapp-exp (incr-prim)
;                                                              (list (var-exp 'v)
;                                                                    (var-exp 'y)))
;                                                 (var-exp 'x)
;                                                 (lit-exp 200))))
;(define un-programa-dificil
;    (a-program empty una-expresion-dificil))




;EJEMPLO CLASES
; class c1  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  let o1 = new c1() in send o1 m1()



(interpretador)