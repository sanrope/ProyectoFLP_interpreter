#lang eopl
(require racket/local)
;^;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

(define the-grammar
  '((program ((arbno class-decl) expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)   
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
   ; (expression
    ; ("if" expression "then" expression "elseif" expression "then" expression "else" expression)
   ;   elseif-exp)
   (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("set" identifier ":=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     substract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("/") div-prim)
    (primitive (">") moreThan-prim)
    (primitive ("<") lessThan-prim)
    (primitive (">=") moreOrEqualThan-prim)
    (primitive ("<=") lessOrEqualThan-prim)
    (primitive ("=") equal-prim)
    (primitive ("max") max-prim)
    (primitive ("min") min-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
    ;(primitive ("zero?") zero-test-prim)
    (primitive ("list") list-prim)
    (primitive ("cons") cons-prim)
    ;(primitive ("nil")  nil-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)
    (primitive ("null?") null-prim)




    
;^;;;;;;;;;;;;;;; new productions for oop ;;;;;;;;;;;;;;;;

    
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
      ("class." expression "." identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)


    

;^;;;;;;;;;;;;;;; end new productions for oop ;;;;;;;;;;;;;;;;

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;^;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls) ;\new1
        (eval-expression exp (empty-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      ;;;;
     ; (elseif-exp (test-exp true-exp second-test-exp second-true-exp  false-exp)
         ;         (if (true-value? (eval-expression test-exp env))
      ;                (eval-expression true-exp env)
         ;             (if (true-value? (eval-expression second-test-exp env))
          ;                (eval-expression second-true-exp env)
          ;                (eval-expression false-exp env))))
      ;;;;
      (let-exp (ids rands body)
               (let ((args (eval-let-exp-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands      rands env)))
                 (if (procval? proc)
                     (apply-procval proc args)
                     (eopl:error 'eval-expression 
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (varassign-exp (id rhs-exp)
                     (setref!
                      (apply-env-ref env id)
                      (eval-expression rhs-exp env))
                     1)
;&
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
;^;;;;;;;;;;;;;;; begin new cases for chap 5 ;;;;;;;;;;;;;;;;
      
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (begin
            (find-method-and-apply
              'initialize class-name obj args)
             obj)))
      
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (part->class-name obj) obj args)))
          ;args))
      
;^;;;;;;;;;;;;;;; end new cases for chap 5 ;;;;;;;;;;;;;;;;
      )))
      

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

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


(define make-list-of-direct-target
  (lambda (list)
    (if (null? list)
        empty
        (cons (direct-target (car list)) (make-list-of-direct-target (cdr list))))))


(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (suma args 0));funciona
      (substract-prim () (division-resta - args));funciona
      (mult-prim () (if (null? args)
                        (eopl:error 'apply-primitive "empty list ~s" args)
                        (multiplicacion args 1)));funciona
      (div-prim() (division-resta / args));funciona
      (incr-prim () (add1-sub1 + args));funciona
      (decr-prim () (add1-sub1 - args));funciona
      (moreThan-prim() (mayor-menor-igual args >));funciona
      (lessThan-prim() (mayor-menor-igual args <));funciona
      (lessOrEqualThan-prim() (mayor-menor-igual args <=));funciona
      (moreOrEqualThan-prim() (mayor-menor-igual args >=));funciona
      (equal-prim() (mayor-menor-igual args equal?));funciona      
      (max-prim() ((car (reverse (insertionSort args)))));funciona
      (min-prim() (car (insertionSort args)));funciona   
      (and-prim() (mayor-menor-igual args (lambda (u v) (and u v))));funciona
      (or-prim() (mayor-menor-igual args (lambda (u v) (or u v))));funciona
      (not-prim() (if (= (length args) 1)
                      (not (true-value? (car args)))
                      (eopl:error 'apply-primitive "just valid for 1 value, given values ~s" (length args))
                      ))
      (list-prim() args)
      (cons-prim() (cons (car args) (car (cdr args))))
      (car-prim() (if (> (length args) 1)
                      (eopl:error 'apply-primitive "just valid for 1 value, given values ~s" (length args))
                      (car (car args))
                      ));funciona para listas
      
      (cdr-prim() (if (> (length args) 1)
                      (eopl:error 'apply-primitive "just valid for 1 value, given values ~s" (length args))
                      (cdr (car args))
                      ));funciona
      (null-prim() (null? args))
     ; (zero-test-prim () (if (zero? (car args)) 1 0))
      ;(nil-prim () '())
      )));funciona

;FUNCIONES AUXILIARES

(define lista-cons
  (lambda (args)
    (cond
      [(null? args) empty]
      [(list? args) (cons (car args) (lista-cons (cdr args)))])))

;Suma

(define suma
  (lambda(list-of-numbers aux)
    (if (null? list-of-numbers)
        aux
        (and (set! aux (+ (car list-of-numbers) aux)) (suma (cdr list-of-numbers) aux)))))

(define multiplicacion
  (lambda(list-of-numbers aux)
    (if (null? list-of-numbers)
        aux
        (and (set! aux (* (car list-of-numbers) aux)) (multiplicacion (cdr list-of-numbers) aux)))))


(define add1-sub1
  (lambda (sym list-of-numbers)
    (if(or(null? list-of-numbers) (> (length list-of-numbers) 1))
       (eopl:error 'add1-sub1 "just valid for 1 value, given values ~s " (length list-of-numbers))
       (sym (car list-of-numbers) 1))))

;Insertion sort

(define insertionSort
  (lambda (list)
    (if (null? list)
        '()
        (insert (car list)
                (insertionSort (cdr list))
        )
    )
  )
)

(define insert
  (lambda (sym sortedList)
    (if (null? sortedList)
        (list sym)
        (if (<= sym (car sortedList))
            (cons sym sortedList)
            (cons (car sortedList) (insert sym (cdr sortedList))
        )
    )
  )
))    



(define division-resta
  (lambda (sym list-of-numbers)
    (local [(define division-resta-aux
              (lambda (list-of-numbers aux-number)
                (if (null? list-of-numbers)
                    aux-number
                    (division-resta-aux (cdr list-of-numbers) (sym aux-number (car list-of-numbers) )))
                )
              )
            ]
      (division-resta-aux (cdr list-of-numbers) (car list-of-numbers))
      )
    )
  )



(define mayor-menor-igual
  (lambda (lista sym)
          (if (> (length lista) 1)
              (local [(define mayor-menor-igual-aux
                        (lambda (lista aux)
                          (if aux
                              (if (pair? (cdr lista))
                                  (mayor-menor-igual-aux (cdr lista) (sym (car lista) (cadr lista)))
                                  aux)
                              #f)
                          )
                        )
                      ]
                (mayor-menor-igual-aux lista #true))
             (eopl:error mayor-menor-igual "empty list ~s" lista)
          )
  ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))


;^;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))


;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name field-ids m-decls)
        class-name))))

;(define class-decl->super-name
;  (lambda (c-decl)
;    (cases class-decl c-decl
;      (a-class-decl (class-name super-name field-ids m-decls)
 ;       super-name))))

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
        
;^;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env))))))
               
;^;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))



;^;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

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

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

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

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec)
    (extended-env-record syms vec (empty-env))))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))




;; evaluar
(define aux
   (lambda (x)
     x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        ;(cons
          (make-object c-decl)))))
          ;(new-object (class-decl->super-name c-decl)))))))


(define make-object
  (lambda (c-decl)
    (a-part
      (class-decl->class-name c-decl)
      (make-vector (length (class-decl->field-ids c-decl)) (direct-target 0)))))




;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
    (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)))))


;;;;;;;;;;;;;;;;MODIFICADO;;;;;;;;;;;;;;;;;;;;;;;;;
(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl)))
      (eval-expression body
        (extend-env
          (cons 'self ids)
          (cons self args)
          (build-field-env 
            self))))))

(define build-field-env
  (lambda (part)
    (if (null? part)
      (empty-env)
      (extend-env-refs
        (part->field-ids part)
         ;(car parts))
        (part->fields part)
        ))))
         ;(car parts))
        ;(empty-env)))))
         ;(cdr parts))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
      
;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

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

;;;;;;;;;;;;;;;; selectors of all sorts ;;;;;;;;;;;;;;;;

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
;  (lambda (part)
;    (class-decl->super-name (part->class-decl part))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

;(define class-name->super-name
;  (lambda (class-name)
 ;   (class-decl->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))

;;

(define read-eval-print 
  (sllgen:make-rep-loop  "-->" eval-program
                         (sllgen:make-stream-parser
                                  the-lexical-spec 
                                  the-grammar)))


;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))



;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (part? x) (list? x))))

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
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))


(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(read-eval-print)
(scan&parse "

;RETORNA 4 POR SER POR REFERENCIA
let
p = proc(x)
set x := 4
in
let
a = 3
in
begin
(p a);
a
end")

;RETORNA 1
(scan&parse "
class c1
field i
field j
method initialize (x)
begin
set x := 1
end
method countup (d)
begin
set i := +(i,d);
set j := -(j,d)
end
method getstate () list(i,j)

let t1 = 0
t2 = 0

in begin
new c1(t1);
t1
end
")

;RETORNA (list 13 12)
(scan&parse "
class c1
field i
field j
method initialize (x)
begin
set i := +(x,3);
set j := +(x,2);
set x := 1

end
method countup (d)
begin
set i := +(i,d);
set j := -(j,d)
end
method getstate () list(i,j)

let t1 = 0
t2 = 0
o = new c1 (10)
in
begin
class.o.getstate()
end
")
;RETORA ESTRUCTURA DE UN OBJETO POR SER (REFERENCIA)
(scan&parse "
class c1
field i
method initialize (x)
set i := x
method countup (d)
set i := +(i,d)
method getstate () i

new c1(3)")


;RETORNA 1 PORQUE EL METODO INITIALIZE MODIFICA LA REFERENCIA t1
(scan&parse "
class c1
field i
field j
method initialize (x)
begin
set i := +(x,3);
set j := +(x,2);
set x := 1

end
method countup (d)
begin
set i := +(i,d);
set j := -(j,d)
end
method getstate () list(i,j)

let t1 = 0
t2 = 0

in
begin
new c1 (t1);
t1
end")



;;Ejemplos 
;; class c1 extends object  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  let o1 = new c1() in send o1 m1()


;;;; class c1 extends object  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  class c2 extends c1  field x field y  method initialize()  begin set x = 2; set y = 3 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()


;;;; class c1 extends object  field x field y  method initialize()  begin   set x = 1; set y = 2 end method m1() x method m2() y  class c2 extends c1  field x field y  method initialize()  begin   super initialize(); set  x = 2; set y = 3 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()

;;class c1 extends object  field x field y  method initialize()  begin   set x = 1; set y = 2 end method m1() x method m2() send self m1()  class c2 extends c1  field x field y  method initialize()  begin   super initialize(); set  x = 9; set y = 10 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()