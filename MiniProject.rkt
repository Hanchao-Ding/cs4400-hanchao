#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mini Project: STLC with Named Exceptions
;; Language name: ExnSTLC
;;
;; Features:
;; - numbers
;; - addition
;; - variables
;; - lambda
;; - function application
;; - named exceptions
;; - try/catch
;; - static type checking

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(define-type Type
  [numT]
  [funT (arg : Type) (ret : Type)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define-type Expr
  [numE (n : Number)]
  [addE (left : Expr) (right : Expr)]
  [divE (left : Expr) (right : Expr)]

  [idE (name : Symbol)]
  [lamE (param : Symbol) (param-type : Type) (body : Expr)]
  [appE (fun-expr : Expr) (arg-expr : Expr)]

  ;; raise has a result type annotation
  ;; Example: (raiseE 'DivZero (numT))
  [raiseE (code : Symbol) (result-type : Type)]

  ;; try body catch exception-name => handler
  [tryE (body : Expr) (code : Symbol) (handler : Expr)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Values

(define-type Value
  [numV (n : Number)]
  [closV (param : Symbol)
         (body : Expr)
         (env : Env)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation Result
;;
;; Evaluation may either:
;; - produce a normal value
;; - raise a named exception

(define-type Result
  [okR (v : Value)]
  [raiseR (code : Symbol)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments

(define-type-alias Env (Symbol -> Value))
(define-type-alias TypeEnv (Symbol -> Type))
(define-type-alias ExnEnv (Listof Symbol))

(define (empty-env [name : Symbol]) : Value
  (error 'interp "unbound variable"))

(define (extend-env [env : Env] [name : Symbol] [val : Value]) : Env
  (lambda (query)
    (if (symbol=? query name)
        val
        (env query))))

(define (empty-tenv [name : Symbol]) : Type
  (error 'typecheck "unbound variable"))

(define (extend-tenv [tenv : TypeEnv] [name : Symbol] [ty : Type]) : TypeEnv
  (lambda (query)
    (if (symbol=? query name)
        ty
        (tenv query))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exception Environment Helpers

(define (exn-member? [code : Symbol] [exns : ExnEnv]) : Boolean
  (type-case (Listof Symbol) exns
    [empty #false]
    [(cons first rest)
     (if (symbol=? code first)
         #true
         (exn-member? code rest))]))

(define default-exns : ExnEnv
  (list 'DivZero 'BadInput 'Fail 'NotFound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Equality

(define (type=? [t1 : Type] [t2 : Type]) : Boolean
  (type-case Type t1
    [(numT)
     (type-case Type t2
       [(numT) #true]
       [(funT a r) #false])]

    [(funT a1 r1)
     (type-case Type t2
       [(numT) #false]
       [(funT a2 r2)
        (and (type=? a1 a2)
             (type=? r1 r2))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checker
;;
;; Main idea:
;; - numbers have Num
;; - addition requires Num + Num
;; - lambda has function type
;; - application checks argument type
;; - raiseE is allowed only if the exception name is declared
;; - tryE requires body and handler to have the same type

(define (typecheck [expr : Expr]) : Type
  (typecheck/env expr empty-tenv default-exns))

(define (typecheck/env [expr : Expr] [tenv : TypeEnv] [exns : ExnEnv]) : Type
  (type-case Expr expr

    [(numE n)
     (numT)]

    [(addE left right)
     (let ([lt (typecheck/env left tenv exns)]
           [rt (typecheck/env right tenv exns)])
       (if (and (type=? lt (numT))
                (type=? rt (numT)))
           (numT)
           (error 'typecheck "addition expects two numbers")))]

    [(divE left right)
     (let ([lt (typecheck/env left tenv exns)]
           [rt (typecheck/env right tenv exns)])
       (if (and (type=? lt (numT))
                (type=? rt (numT)))
           (numT)
           (error 'typecheck "division expects two numbers")))]

    [(idE name)
     (tenv name)]

    [(lamE param param-type body)
     (funT param-type
           (typecheck/env body
                          (extend-tenv tenv param param-type)
                          exns))]

    [(appE fun-expr arg-expr)
     (let ([fun-type (typecheck/env fun-expr tenv exns)]
           [arg-type (typecheck/env arg-expr tenv exns)])
       (type-case Type fun-type
         [(funT expected-arg ret-type)
          (if (type=? expected-arg arg-type)
              ret-type
              (error 'typecheck "function argument type mismatch"))]
         [else
          (error 'typecheck "application expects a function")]))]

    [(raiseE code result-type)
     (if (exn-member? code exns)
         result-type
         (error 'typecheck "unknown exception name"))]

    [(tryE body code handler)
     (if (exn-member? code exns)
         (let ([body-type (typecheck/env body tenv exns)]
               [handler-type (typecheck/env handler tenv exns)])
           (if (type=? body-type handler-type)
               body-type
               (error 'typecheck "try body and handler must have same type")))
         (error 'typecheck "unknown exception name"))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter
;;
;; Evaluation returns Result:
;; - okR value
;; - raiseR exception-name
;;
;; Exceptions propagate outward until caught by tryE.

(define (interp [expr : Expr]) : Result
  (interp/env expr empty-env))

(define (interp/env [expr : Expr] [env : Env]) : Result
  (type-case Expr expr

    [(numE n)
     (okR (numV n))]

    [(addE left right)
     (type-case Result (interp/env left env)
       [(raiseR code)
        (raiseR code)]

       [(okR lv)
        (type-case Result (interp/env right env)
          [(raiseR code)
           (raiseR code)]

          [(okR rv)
           (type-case Value lv
             [(numV ln)
              (type-case Value rv
                [(numV rn)
                 (okR (numV (+ ln rn)))]
                [else
                 (error 'interp "right side of addition is not a number")])]
             [else
              (error 'interp "left side of addition is not a number")])])])]

    [(divE left right)
     (type-case Result (interp/env left env)
       [(raiseR code)
        (raiseR code)]

       [(okR lv)
        (type-case Result (interp/env right env)
          [(raiseR code)
           (raiseR code)]

          [(okR rv)
           (type-case Value lv
             [(numV ln)
              (type-case Value rv
                [(numV rn)
                 (if (= rn 0)
                     (raiseR 'DivZero)  
                     (okR (numV (/ ln rn))))]
                [else
                 (error 'interp "right side of division is not a number")])]
             [else
              (error 'interp "left side of division is not a number")])])])]

    [(idE name)
     (okR (env name))]

    [(lamE param param-type body)
     (okR (closV param body env))]

    [(appE fun-expr arg-expr)
     (type-case Result (interp/env fun-expr env)
       [(raiseR code)
        (raiseR code)]

       [(okR fun-val)
        (type-case Result (interp/env arg-expr env)
          [(raiseR code)
           (raiseR code)]

          [(okR arg-val)
           (type-case Value fun-val
             [(closV param body clos-env)
              (interp/env body
                          (extend-env clos-env param arg-val))]
             [else
              (error 'interp "application expects a function")])])])]

    [(raiseE code result-type)
     (raiseR code)]

    [(tryE body code handler)
     (type-case Result (interp/env body env)
       [(okR v)
        (okR v)]

       [(raiseR raised-code)
        (if (symbol=? raised-code code)
            (interp/env handler env)
            (raiseR raised-code))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run helper
;;
;; This checks the program first, then interprets it.

(define (run [expr : Expr]) : Result
  (begin
    (typecheck expr)
    (interp expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Programs

;; 1 + 2
(define ex-add
  (addE (numE 1) (numE 2)))

;; ((lambda x : Num. x + 1) 5)
(define ex-lambda
  (appE
   (lamE 'x
         (numT)
         (addE (idE 'x) (numE 1)))
   (numE 5)))

;; try raise DivZero catch DivZero => 42
(define ex-caught
  (tryE
   (raiseE 'DivZero (numT))
   'DivZero
   (numE 42)))

;; try raise BadInput catch DivZero => 42
;; This does not catch BadInput, so BadInput propagates.
(define ex-uncaught
  (tryE
   (raiseE 'BadInput (numT))
   'DivZero
   (numE 42)))

;; 10 + (try raise DivZero catch DivZero => 5)
(define ex-exn-inside-add
  (addE
   (numE 10)
   (tryE
    (raiseE 'DivZero (numT))
    'DivZero
    (numE 5))))

;; Function that raises an exception
;; ((lambda x : Num. raise Fail : Num) 10)
(define ex-function-raises
  (appE
   (lamE 'x
         (numT)
         (raiseE 'Fail (numT)))
   (numE 10)))

;; Catch exception raised inside function call
(define ex-catch-function-raise
  (tryE
   ex-function-raises
   'Fail
   (numE 999)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bad Programs for Type Checker

;; 1 applied as a function
(define bad-apply-number
  (appE (numE 1) (numE 2)))

;; Function expects Num, but receives function
(define bad-arg-type
  (appE
   (lamE 'x
         (numT)
         (addE (idE 'x) (numE 1)))
   (lamE 'y (numT) (idE 'y))))

;; try body has Num, handler has Num -> okay
;; If you want a bad try example, handler must have different type.
(define bad-try-type
  (tryE
   (raiseE 'Fail (numT))
   'Fail
   (lamE 'x (numT) (idE 'x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(test (typecheck ex-add)
      (numT))

(test (run ex-add)
      (okR (numV 3)))

(test (typecheck ex-lambda)
      (numT))

(test (run ex-lambda)
      (okR (numV 6)))

(test (typecheck ex-caught)
      (numT))

(test (run ex-caught)
      (okR (numV 42)))

(test (typecheck ex-uncaught)
      (numT))

(test (run ex-uncaught)
      (raiseR 'BadInput))

(test (run ex-exn-inside-add)
      (okR (numV 15)))

(test (run ex-function-raises)
      (raiseR 'Fail))

(test (run ex-catch-function-raise)
      (okR (numV 999)))

(test/exn (typecheck bad-apply-number)
          "application expects a function")

(test/exn (typecheck bad-arg-type)
          "function argument type mismatch")

(test/exn (typecheck bad-try-type)
          "try body and handler must have same type")


(test
 (run (divE (numE 10) (numE 2)))
 (okR (numV 5)))


(test
 (run (divE (numE 10) (numE 0)))
 (raiseR 'DivZero))


(test
 (run
  (tryE
   (divE (numE 10) (numE 0))
   'DivZero
   (numE 999)))
 (okR (numV 999)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demo Tests for Presentation

;; Demo 1: normal addition
(test
 (run (addE (numE 1) (numE 2)))
 (okR (numV 3)))

;; Demo 2: handled exception
(test
 (run
  (tryE
   (raiseE 'DivZero (numT))
   'DivZero
   (numE 42)))
 (okR (numV 42)))

;; Demo 3: unhandled exception
(test
 (run
  (tryE
   (raiseE 'BadInput (numT))
   'DivZero
   (numE 42)))
 (raiseR 'BadInput))

;; Demo 4: exception inside addition
(test
 (run
  (addE
   (numE 10)
   (tryE
    (raiseE 'DivZero (numT))
    'DivZero
    (numE 5))))
 (okR (numV 15)))

;; Demo 5: function call
(test
 (run
  (appE
   (lamE 'x
         (numT)
         (addE (idE 'x) (numE 1)))
   (numE 5)))
 (okR (numV 6)))

;; Demo 6: function raises exception, outside try catches it
(test
 (run
  (tryE
   (appE
    (lamE 'x
          (numT)
          (raiseE 'Fail (numT)))
    (numE 10))
   'Fail
   (numE 999)))
 (okR (numV 999)))

;; Demo 7: nested try/catch
(test
 (run
  (tryE
   (tryE
    (raiseE 'BadInput (numT))
    'DivZero
    (numE 1))
   'BadInput
   (numE 2)))
 (okR (numV 2)))

;; Demo 8: try/catch returns a function
(test
 (typecheck
  (tryE
   (raiseE 'Fail (funT (numT) (numT)))
   'Fail
   (lamE 'x (numT) (addE (idE 'x) (numE 1)))))
 (funT (numT) (numT)))

;; Demo 9: returned function is applied to 10
(test
 (run
  (appE
   (tryE
    (raiseE 'Fail (funT (numT) (numT)))
    'Fail
    (lamE 'x (numT) (addE (idE 'x) (numE 1))))
   (numE 10)))
 (okR (numV 11)))

;; Demo 10: invalid unknown exception
(test/exn
 (typecheck
  (raiseE 'RandomError (numT)))
 "unknown exception name")

;; Demo 11: invalid try/catch type mismatch
(test/exn
 (typecheck
  (tryE
   (raiseE 'Fail (numT))
   'Fail
   (lamE 'x (numT) (idE 'x))))
 "try body and handler must have same type")