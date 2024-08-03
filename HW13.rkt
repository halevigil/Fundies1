;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; SIMPLE TYPED LANGUAGE
 
; An Expression is a:
; - Number
; - Boolean
; - 'unit
; - (list AopName Expression Expression)
; - (list BopName Expression Expression)
; - (list CmpopName Expression Expression)
; - (list 'if Expression Expression Expression)
; - (list 'var Symbol)
; - (list 'lam Symbol Type Expression)
; - (list 'app Expression Expression)
; - (list 'pair Expression Expression)
; - (list 'fst Expression)
; - (list 'snd Expression)
; - (list 'inleft Expression Type Type)
; - (list 'inright Expression Type Type)
; - (list 'case Expression Expression Expression)


(define AOPS '(+ - *))
; An AopName is a member of AOPS, all of which have type: Number Number -> Number
(define BOPS '(and or))
; An BopName is a member of BOPS, all of which have type: Boolean Boolean -> Boolean
(define CmpOPS '(> < =))
; An CmpopName is a member of CmpOPS, all of which have type: Number Number -> Boolean
 
; A Type is one of:
; - 'Number
; - 'Boolean
; - 'Unit
(define-struct funty [arg ret])
; - (make-funty Type Type)
(define-struct pairty [fst snd])
; - (make-pairty Type Type)
(define-struct sumty [left right])
; - (make-sumty Type Type)
 
(define (Fun ta tr) (make-funty ta tr))
 
; ensuretype : Environment Expression Type -> Boolean
; Check that expression e has type t, error if e's type does not match t
(define (ensuretype env e t)
  (local ((define ty-e (typecheck-env env e)))
    (if (equal? ty-e t)
        #true
        (error "Expression " e " has type " ty-e " but was expected to have type " t))))
 
(define-struct var:ty [var ty])
; An Environment is a [List of (make-var:ty Symbol Type)]
; Interp: A mapping from variables to types
 
; typecheck-env: Environment Expression -> Type
; returns the type of the expression e or errors if the expression is ill-typed
(define (typecheck-env env e)
  (cond [(number? e) 'Number]
        [(boolean? e) 'Boolean]
        [(and (symbol? e) (symbol=? e 'unit)) 'Unit]
        [(member? (first e) AOPS)
         (local [(define ty-1 (ensuretype env (second e) 'Number))
                 (define ty-2 (ensuretype env (third e) 'Number))]
           'Number)]
        [(member? (first e) BOPS)
         (local [(define ty-1 (ensuretype env (second e) 'Boolean))
                 (define ty-2 (ensuretype env (third e) 'Boolean))]
           'Boolean)]
        [(member? (first e) CmpOPS)
         (local [(define ty-1 (ensuretype env (second e) 'Number))
                 (define ty-2 (ensuretype env (third e) 'Number))]
           'Boolean)]
        [(symbol=? (first e) 'if)
         (local [(define ty-0 (ensuretype env (second e) 'Boolean))
                 (define ty-1 (typecheck-env env (third e)))
                 (define ty-2 (typecheck-env env (fourth e)))]
           (if (equal? ty-1 ty-2)
               ty-1
               (error "Branches of if expression " e "do not match")))]
        [(symbol=? (first e) 'var)
         (lookup-env env (second e))]
        [(symbol=? (first e) 'lam)
         (Fun (third e)
              (typecheck-env (upd-env env (second e) (third e))
                             (fourth e)))]
        [(symbol=? (first e) 'app)
         (local [(define ty-e1 (typecheck-env env (second e)))
                 (define ty-e2 (typecheck-env env (third e)))]
           (if (not (funty? ty-e1))
               (error "Trying to apply a non-function expression " (second e)
                      " of type " ty-e1)
               (if (equal? (funty-arg ty-e1) ty-e2)
                   (funty-ret ty-e1)
                   (error "Trying to apply a function of type " ty-e1
                          " to an argument of type " ty-e2))))]
        [(symbol=? (first e) 'pair)
         (local [(define ty-e1 (typecheck-env env (second e)))
                 (define ty-e2 (typecheck-env env (third e)))]
           (make-pairty ty-e1 ty-e2))]
        [(symbol=? (first e) 'fst)
         (local [(define ty-e1 (typecheck-env env (second e)))]
           (if (not (pairty? ty-e1))
               (error "trying to get call fst on expression " (second e)
                      " with type " ty-e1 ", expected pair")
               (pairty-fst ty-e1)))]
        [(symbol=? (first e) 'snd)
         (local [(define ty-e1 (typecheck-env env (second e)))]
           (if (not (pairty? ty-e1))
               (error "trying to get call snd on expression " (second e)
                      " with type " ty-e1 ", expected pair")
               (pairty-snd ty-e1)))]
        [(symbol=? (first e) 'inleft)
         (local [(define ty-e1 (typecheck-env env (second e)))]
           (if (equal? ty-e1 (third e))
               (make-sumty (third e) (fourth e))
               (error "Trying to put inleft an expression of type " ty-e1
                      " to a sum type with left type " (third e))))]
        [(symbol=? (first e) 'inright)
         (local [(define ty-e1 (typecheck-env env (second e)))]
           (if (equal? ty-e1 (fourth e))
               (make-sumty (third e) (fourth e))
               (error "Trying to put inright an expression of type " ty-e1
                      " to a sum type with right type " (fourth e))))]
        [(symbol=? (first e) 'case)
         (local [(define ty-e1 (typecheck-env env (second e)))
                 (define ty-e2 (typecheck-env env (third e)))
                 (define ty-e3 (typecheck-env env (fourth e)))]
           (if (not (sumty? ty-e1))
               (error "Expression " (second e) " has type " ty-e1 ", expected sumty")
               (cond
                 [(not (funty? ty-e2)) (error "case left expression "
                                              (third e) " is not a function")]
                 [(not (funty? ty-e3)) (error "case right expression "
                                              (fourth e) " is not a function")]
                 [(and (funty? ty-e2) (funty? ty-e3))
                  (if (not (equal? (funty-ret ty-e2) (funty-ret ty-e3)))
                      (error "case left expression " (third e) " and right expression " (fourth e)
                             " have different return types")
                      (cond
                        [(not (equal? (sumty-left ty-e1) (funty-arg ty-e2)))
                         (error "case left expression " (third e) " has input type "
                                (funty-arg ty-e2) " different than sumty left type "
                                (sumty-left ty-e1))]
                        [(not (equal? (sumty-right ty-e1) (funty-arg ty-e3)))
                         (error "case right expression " (fourth e) " has input type "
                                (funty-arg ty-e3) " different than sumty right type "
                                (sumty-right ty-e1))]
                        [else (funty-ret ty-e2)]))])))]))
         
 
; upd-env : Environment Symbol Type -> Environment
; Adds a variable:type binding to the environment env
(define (upd-env env var ty)
  (cons (make-var:ty var ty) env))
 
; lookup-env : Environment Symbol -> Type
; lookup the type associated with var in env
(define (lookup-env env var)
  (cond [(empty? env) (error "Variable not found " var)]
        [(cons? env) (if (symbol=? (var:ty-var (first env)) var)
                         (var:ty-ty (first env))
                         (lookup-env (rest env) var))]))
 
(check-error (lookup-env '() 'x))
(check-expect (lookup-env (list (make-var:ty 'a 'Number)
                                (make-var:ty 'c 'Boolean))
                          'c)
              'Boolean)
(check-expect (lookup-env (list (make-var:ty 'a 'Number)
                                (make-var:ty 'c 'Boolean)
                                (make-var:ty 'a 'Boolean))
                          'a)
              'Number)
; typecheck : Expression -> Type
; returns the type of the expression e (with an empty environment)
; or errors if the expression is ill-typed
(define (typecheck e)
  (typecheck-env empty e))
 
(check-expect (typecheck 1) 'Number)
(check-expect (typecheck #false) 'Boolean)
(check-expect (typecheck (list '+ 1 2)) 'Number)
(check-expect (typecheck (list '- (list '+ 1 2) 5)) 'Number)
(check-error (typecheck (list '+ #false 2)))
 
(check-error (typecheck (list 'if 1 #true #false)))
(check-error (typecheck '(if (> 3 9) 1 #true)))
(check-expect (typecheck '(if (> 3 9) 1 4)) 'Number)
(check-expect (typecheck '(and #false (> 2 2))) 'Boolean)
(check-error (typecheck '(and #false (+ 2 2))))

; Exercise 1
(check-expect (typecheck (list 'lam 'x 'Number (list '+ (list 'var 'x) 1)))
              (make-funty 'Number 'Number))
(check-expect (typecheck (list 'lam 'x 'Boolean (list 'or (list 'var 'x) #f)))
              (make-funty 'Boolean 'Boolean))
(check-expect (typecheck (list 'lam 'x 'Boolean
                               (list 'if (list 'var 'x)
                                     (list 'and (list 'var 'x) #t) (list 'var 'x))))
              (make-funty 'Boolean 'Boolean))
(check-expect (typecheck (list 'lam 'x 'Number
                               (list 'lam 'b 'Number (list '< (list 'var 'x) (list 'var 'b)))))
              (make-funty 'Number (make-funty 'Number 'Boolean)))
(check-error (typecheck (list 'lam 'x 'Boolean (list '+ (list 'var 'x) 1)))
             "Expression (list 'var 'x) has type 'Boolean but was expected to have type 'Number")
(check-error (typecheck (list 'lam 'x 'Number (list '+ (list 'var 'x) #f)))
             "Expression #false has type 'Boolean but was expected to have type 'Number")
(check-error (typecheck (list 'lam 'x 'Boolean (list '+ (list 'var 'x) #f)))
             "Expression (list 'var 'x) has type 'Boolean but was expected to have type 'Number")

(check-expect (typecheck (list 'app (list 'lam 'x 'Number (list '+ (list 'var 'x) 1)) 0)) 'Number)
(check-expect (typecheck (list 'app (list 'lam 'x 'Number
                                          (list 'lam 'b 'Number (list '< (list 'var 'b)
                                                                      (list 'var 'x)))) 2))
              (make-funty 'Number 'Boolean))
(check-error (typecheck (list 'app (list '+ 3 4) 5))
             "Trying to apply a non-function expression (list '+ 3 4) of type 'Number")
(check-error
 (typecheck (list 'app (list 'lam 'x 'Number (list '+ (list 'var 'x) 1)) #f))
 "Trying to apply a function of type (make-funty 'Number 'Number) to an argument of type 'Boolean")
(check-error (typecheck (list 'app (list 'lam 'x 'Boolean (list '+ (list 'var 'x) 1)) 0))
             "Expression (list 'var 'x) has type 'Boolean but was expected to have type 'Number")



(check-expect (typecheck-env (list (make-var:ty 'x 'Boolean))
                             (list 'lam 'x 'Number (list '+ (list 'var 'x) 1)))
              (make-funty 'Number 'Number))
(check-expect (typecheck-env (list (make-var:ty 'x 'Boolean) (make-var:ty 'c 'Number)
                                   (make-var:ty 'c 'Boolean))
                             (list 'app (list 'lam 'x 'Number
                                              (list 'lam 'b 'Number
                                                    (list '< (list '+ '(var b) '(var c))
                                                          '(var x)))) 2))
              (make-funty 'Number 'Boolean))



; Exercise 2
(check-expect (typecheck (list 'pair 0 #t)) (make-pairty 'Number 'Boolean))
(check-expect (typecheck (list 'pair
                               (list 'app (list 'lam 'x 'Number
                                                (list 'lam 'b 'Number
                                                      (list '< (list 'var 'x) (list 'var 'b)))) 2)
                               (list 'lam 'x 'Boolean (list 'if (list 'var 'x)
                                                            (list 'and (list 'var 'x) #t)
                                                            (list 'var 'x)))))
              (make-pairty (make-funty 'Number 'Boolean) (make-funty 'Boolean 'Boolean)))
(check-error (typecheck (list 'pair
                              (list 'lam 'x 'Boolean (list '+ (list 'var 'x) 1))
                              (list 'app (list 'lam 'x 'Number (list '+ (list 'var 'x) 1)) 0)))
             "Expression (list 'var 'x) has type 'Boolean but was expected to have type 'Number")
(check-error (typecheck (list 'pair (list '- 3 2) (list '+ #t 0)))
             "Expression #true has type 'Boolean but was expected to have type 'Number")

(check-expect (typecheck '(fst (pair #t 4))) 'Boolean)
(check-expect (typecheck '(snd (pair #t 4))) 'Number)
(check-expect (typecheck '(lam x Number (and (fst (pair #t 4))
                                             (< (var x) (snd (pair #f 2))))))
              (make-funty 'Number 'Boolean))
(check-error (typecheck '(fst 2))
             "trying to get call fst on expression 2 with type 'Number, expected pair")
(check-error (typecheck '(snd 2))
             "trying to get call snd on expression 2 with type 'Number, expected pair")

(check-expect (typecheck-env (list (make-var:ty 'b 'Boolean))
                             '(lam x Number (and (fst (pair (and #t (var b)) 4))
                                                 (< (var x) (snd (pair (var b) 2))))))
              (make-funty 'Number 'Boolean))






; Exercise 3

(check-expect (typecheck (list 'inleft 1 'Number 'Number)) (make-sumty 'Number 'Number))
(check-expect (typecheck (list 'inright '(> 3 (- 4 3)) 'Number 'Boolean))
              (make-sumty 'Number 'Boolean))
(check-error
 (typecheck (list 'inright 1 'Number 'Boolean))
 "Trying to put inright an expression of type 'Number to a sum type with right type 'Boolean")
(check-error
 (typecheck (list 'inleft '(> 3 (- 4 3)) 'Number 'Boolean))
 "Trying to put inleft an expression of type 'Boolean to a sum type with left type 'Number")

(check-expect (typecheck '(case (inleft 1 Number Number)
                            (lam x Number (+ (var x) 1)) (lam y Number 2))) 'Number)
(check-expect (typecheck '(case (inleft 1 Number Boolean)
                            (lam x Number (> (var x) (- 1 2))) (lam x Boolean (var x)))) 'Boolean)
(check-expect (typecheck '(case (inleft 1 Number Boolean)
                            (lam x Number (lam z Number (> (var z) (var x))))
                            (lam y Boolean (lam z Number (if (var y) #t (> (var z) 3))))))
              (make-funty 'Number 'Boolean))

(check-error
 (typecheck '(case (inleft 1 Boolean Number) (lam x Boolean #t) (lam x Number #t)))
 "Trying to put inleft an expression of type 'Number to a sum type with left type 'Boolean")
(check-error (typecheck '(case 2 (lam x Boolean #t) (lam x Number #t)))
             "Expression 2 has type 'Number, expected sumty")
(check-error (typecheck '(case (inleft 1 Number Boolean) 3 (lam x Number #t)))
             "case left expression 3 is not a function")
(check-error (typecheck '(case (inleft 1 Number Boolean) (lam x Number #t) 2))
             "case right expression 2 is not a function")
(check-error
 (typecheck '(case (inleft 1 Number Boolean) (lam x Number #t) (lam y Boolean 0)))
 (string-append "case left expression (list 'lam 'x 'Number #true) "
                "and right expression (list 'lam 'y 'Boolean 0) have different return types"))
(check-error
 (typecheck '(case (inleft 1 Number Boolean) (lam x Boolean (if (var x) 2 3)) (lam y Boolean 2)))
 (string-append
  "case left expression (list 'lam 'x 'Boolean (list 'if (list 'var 'x) 2 3))"
  " has input type 'Boolean different than sumty left type 'Number"))
(check-error
 (typecheck '(case (inleft 1 Number Boolean) (lam x Number 0) (lam y Number (+ (var y) 2))))
 (string-append "case right expression (list 'lam 'y 'Number (list '+ (list 'var 'y) 2))"
                " has input type 'Number different than sumty right type 'Boolean"))

(check-expect (typecheck-env (list (make-var:ty 'a 'Number))
                             '(case (inleft (var a) Number Boolean)
                                (lam x Number (+ (var x) (var a)))
                                (lam a Boolean (if (var a) 1 0)))) 'Number)

;Exercise 4

(check-expect (typecheck 'unit) 'Unit)
(check-expect (typecheck '(lam x Boolean unit)) (make-funty 'Boolean 'Unit))
(check-error (typecheck '(+ unit 1))
             "Expression 'unit has type 'Unit but was expected to have type 'Number")

;Exercise 5

(define TRUE '(inleft unit Unit Unit))
(define FALSE '(inright unit Unit Unit))
;the following expression is equivalent to (if #t 1 0):
(check-expect (typecheck '(case (inleft unit Unit Unit) (lam x Unit 1) (lam x Unit 0))) 'Number)
;the following expression is equivalent to (if #f 1 0):
(check-expect (typecheck '(case (inright unit Unit Unit) (lam x Unit 1) (lam x Unit 0))) 'Number)
;the following expression is equivalent to (if #f 1 'unit):
(check-error
 (typecheck '(case (inright unit Unit Unit) (lam x Unit 1) (lam x Unit unit)))
 (string-append "case left expression (list 'lam 'x 'Unit 1) and right expression "
                "(list 'lam 'x 'Unit 'unit) have different return types"))



;Exercise 6

; 
; StackLang ;;
; 
 
; P ::= (i-1,...,i-n)
; i ::= push v | sub | add | mul | if0 P P | call | lam x.P
; v ::= 'x | n | thunk P
; 
; S ::= (v-1, ..., v-n)  (where v-i are closed values)
; 
; (P,S) ==> S'    (corresponds to interp)
; 
; (i,P,S) ==> S'  (corresponds to interp-instr)
 
; A Program is a [List-of Instruction]
 
; An Instruction is a:
; - (list 'push Value)
; - (list 'sub)
; - (list 'add)
; - (list 'mul)
; - (list 'if0 Program Program)
; - (list 'call)
; - (list 'lam Symbol Program)
; - (list 'unwrap)
; - (list 'lt)
 
; A Value is a:
; - Symbol
; - (list 'num Number)
; - (list 'thunk Program)
; - (list 'pair Value Value)
 
; A Stack is a [List-of ClosedValue]
 
; A ClosedValue is a Value with no free variables.
; for a pair, the two contained Values should both be a ClosedValue.
 
; 
; INTERPRETER ;;
; 
 
; interp : Program Stack -> Stack
; runs a program with a given starting stack, producing the final stack or erroring.
(define (interp prog stk)
  (cond [(empty? prog) stk]
        [(cons? prog) (interp-instr (first prog) (rest prog) stk)]))
 
(check-expect (interp (list '(push (num 2)))
                      (list))
              (list '(num 2)))
(check-expect (interp (list '(push (num 2)) '(push (num 1)) '(sub))
                      (list))
              (list '(num -1)))
(check-error (interp '((push x))
                     (list)))
 
(check-expect (interp (list '(push (thunk ((push (num 1))))) '(call))
                      (list))
              (list '(num 1)))
 
(check-expect (interp (list '(push (num 0)) '(if0 ((push (num 10))) ((push (num 20)))))
                      (list))
              (list '(num 10)))
 
(define p1 (list '(push (num 1)) '(push (thunk ((lam x ((push x)))))) '(call)))
(check-expect (interp p1 (list))
              (list '(num 1)))
(define p2 (list '(push (num 2)) '(push (num 1)) '(push (num 3))
                 '(push (thunk ((lam 'x ((sub)))))) '(call)))
(check-expect (interp p2 (list))
              (list '(num -1)))
 
; interp-instr : Instruction Program Stack -> Stack
; runs an instruction with a stack and rest of program, producing the final stack or erroring.
(define (interp-instr i prog stk)
  (cond
    [(symbol=? (first i) 'push)
     (if (symbol? (second i))
         (error "Trying to interp a free variable: " (second i))
         (interp prog (cons (second i) stk)))]
    [(symbol=? (first i) 'sub) (interp-sub prog stk)]
    [(symbol=? (first i) 'add) (interp-add prog stk)]
    [(symbol=? (first i) 'mul) (interp-mul prog stk)]
    [(symbol=? (first i) 'if0) (interp-if0 (second i) (third i) prog stk)]
    [(symbol=? (first i) 'call) (interp-call prog stk)]
    [(symbol=? (first i) 'lam) (interp-lam (second i) (third i) prog stk)]
    [(symbol=? (first i) 'unwrap) (interp-unwrap prog stk)]
    [(symbol=? (first i) 'lt) (interp-lt prog stk)]))
 
(check-expect (interp-instr '(sub)
                            (list)
                            '((num 1) (num 2)))
              (list '(num -1)))
(check-expect (interp-instr '(push (num 1))
                            (list '(sub))
                            '((num 1)))
              (list '(num 0)))
(check-expect (interp-instr '(push (num 1))
                            (list)
                            '((num 1)))
              '((num 1) (num 1)))
(check-expect (interp-instr '(mul)
                            '()
                            '((num 1) (num 1) (num 1)))
              '((num 1) (num 1)))
(check-expect (interp-instr '(unwrap) '((sub)) '((pair (num 2) (num 3)))) '((num -1)))
(check-expect (interp-instr '(lt)
                            '()
                            '((num 0) (num 1) (num 1)))
              '((num 1) (num 1)))

; interp-sub : Program Stack -> Stack
; applies subtraction and then continues running the rest of the program on resulting stack
(define (interp-sub prog stk)
  (cond
    [(< (length stk) 2) (error "calling sub with fewer than 2 elements in the stack")]
    [(not (equal? (first (first stk)) 'num))
     (error "calling sub when first of stack is not 'num")]
    [(not (equal? (first (second stk)) 'num))
     (error "calling sub when second of stack is not 'num")]
    [else (interp
           prog
           (cons (list 'num (- (second (first stk)) (second (second stk))))
                 (rest (rest stk))))]))

(check-expect (interp-sub '() '((num 4) (num 3))) '((num 1)))
(check-expect (interp-sub '((add)) '((num 1) (num 2) (num 2) (num 2))) '((num 1) (num 2)))
(check-error (interp-sub '() '((num 3))) "calling sub with fewer than 2 elements in the stack")
(check-error (interp-sub '() '((num 3) (thunk ((push (num 1))))))
             "calling sub when second of stack is not 'num")
(check-error (interp-sub '() '((thunk ((push (num 1)))) (num 3)))
             "calling sub when first of stack is not 'num")
 
; interp-add : Program Stack -> Stack
; applies addition and then continues running the rest of the program on resulting stack
(define (interp-add prog stk)
  (cond
    [(< (length stk) 2) (error "calling add with fewer than 2 elements in the stack")]
    [(not (equal? (first (first stk)) 'num))
     (error "calling add when first of stack is not 'num")]
    [(not (equal? (first (second stk)) 'num))
     (error "calling add when second of stack is not 'num")]
    [else (interp
           prog
           (cons (list 'num (+ (second (first stk)) (second (second stk))))
                 (rest (rest stk))))]))
(check-expect (interp-add '() '((num 4) (num 3))) '((num 7)))
(check-expect (interp-add '((sub)) '((num -1) (num 3) (num 1) (num 2))) '((num 1) (num 2)))
(check-error (interp-add '() '((num 3))) "calling add with fewer than 2 elements in the stack")
(check-error (interp-add '() '((num 3) (thunk ((push (num 1))))))
             "calling add when second of stack is not 'num")
(check-error (interp-add '() '((thunk ((push (num 1)))) (num 3)))
             "calling add when first of stack is not 'num")

 
; interp-mul : Program Stack -> Stack
; applies multiplication and then continues running the rest of the program on resulting stack
(define (interp-mul prog stk)
  (cond
    [(< (length stk) 2) (error "calling mul with fewer than 2 elements in the stack")]
    [(not (equal? (first (first stk)) 'num)) (error "calling mul when first of stack is not 'num")]
    [(not (equal? (first (second stk)) 'num))
     (error "calling mul when second of stack is not 'num")]
    [else (interp
           prog
           (cons (list 'num (* (second (first stk)) (second (second stk))))
                 (rest (rest stk))))]))
(check-expect (interp-mul '() '((num 4) (num 3))) '((num 12)))
(check-expect (interp-mul '((sub)) '((num -1) (num 3) (num 1) (num 2))) '((num -4) (num 2)))
(check-error (interp-mul '() '((num 3))) "calling mul with fewer than 2 elements in the stack")
(check-error (interp-mul '() '((num 3) (thunk ((push (num 1))))))
             "calling mul when second of stack is not 'num")
(check-error (interp-mul '() '((thunk ((push (num 1)))) (num 3)))
             "calling mul when first of stack is not 'num")

 
; interp-call : Program Stack -> Stack
; pops a Program off the top of the stack and continues running the program, erroring if no thunk.
(define (interp-call prog stk)
  (cond
    [(< (length stk) 1)
     (error "Could not apply 'call, as the stack was empty.")]
    [(or (not (list? (first stk)))
         (not (equal? 'thunk (first (first stk)))))
     (error "Could not apply 'call, as the top of the stack was not a thunk.")]
    [else (interp (append (second (first stk)) prog)
                  (rest stk))]))
 
(check-expect (interp-call (list)
                           (list (list 'thunk '((push (num 1))))))
              (list '(num 1)))
(check-expect (interp-call (list)
                           (list (list 'thunk '((sub))) '(num 2) '(num 1)))
              (list '(num 1)))
(check-error (interp-call (list) (list))
             "Could not apply 'call, as the stack was empty.")
(check-error (interp-call (list) (list (list 'num 1) '(num 2) '(num 1)))
             "Could not apply 'call, as the top of the stack was not a thunk.")

 
; interp-if0 : Program Program Program Stack -> Stack
; pops a number off the stack;
; if number is 0, run thn Program followed by prog on the resulting stack,
; otherwise run els Program and then prog on the resulting stack;
; error if no number on top of stack.
(define (interp-if0 thn els prog stk)
  (if (not (equal? (first (first stk)) 'num))
      (error "calling if0 on non-number")
      (if (zero? (second (first stk)))
          (interp (append thn prog) (rest stk))
          (interp (append els prog) (rest stk)))))


(check-error (interp-if0 '((sub)) '((add)) '() '((thunk ((push (num 0)))) (num 4) (num 3)))
             "calling if0 on non-number")
(check-expect (interp-if0 '((sub)) '((add)) '() '((num 0) (num 4) (num 3))) '((num 1)))
(check-expect (interp-if0 '((sub)) '((add)) '() '((num 1) (num 4) (num 3))) '((num 7)))
(check-expect (interp-if0 '((push (num 1)) (sub))
                          '((push (num 1)) (add))
                          '((add)) '((num 0) (num 3) (num 2) (num 2)))
              '((num 0) (num 2)))
 
; —————————————————-
; interp-lam : Symbol Program Program Stack -> Stack
; pops a value from the stack, substitutes it for x in lambda body,
; and runs body followed by prog on the rest of the stack
(define (interp-lam x body prog stk)
  (cond
    [(< (length stk) 1)
     (error "could not apply 'lam, as the stack was empty")]
    [else (interp (append (substitute x (first stk) body) prog)
                  (rest stk))]))
 
(check-expect (interp-lam 'x (list '(push x)) '()
                          (list '(num 1)))
              (list '(num 1)))
(check-error (interp-lam 'x (list '(push x)) '()
                         (list))
             "could not apply 'lam, as the stack was empty")
 
; substitute : Symbol ClosedValue Program -> Program
; substitutes free occurrences of x with v in prog
(define (substitute x v prog)
  (cond [(empty? prog) prog]
        [(cons? prog) (cons (substitute-instr x v (first prog)) (substitute x v (rest prog)))]))
 
(check-expect (substitute 'x '(num 1) (list '(push x) '(push (num 2))))
              (list '(push (num 1)) '(push (num 2))))
(check-expect (substitute 'x '(num 1) (list '(push x) '(lam x ((push x)))))
              (list '(push (num 1)) '(lam x ((push x)))))
(check-expect (substitute 'x '(num 1) (list '(push (pair x (num 5)))))
              (list '(push (pair (num 1) (num 5)))))
 
; substitute-instr : Symbol ClosedValue Instruction -> Instruction
; substitutes free occurrences of x with v in instruction i
(define (substitute-instr x v i)
  (cond
    [(symbol=? (first i) 'push)
     (list 'push (substitute-val x v (second i)))]
    [(symbol=? (first i) 'sub) i]
    [(symbol=? (first i) 'mul) i]
    [(symbol=? (first i) 'add) i]
    [(symbol=? (first i) 'if0)
     (list 'if0 (substitute x v (second i))
           (substitute x v (third i)))]
    [(symbol=? (first i) 'call) i]
    [(symbol=? (first i) 'lam)
     (if (not (symbol=? (second i) x))
         (list 'lam (second i) (substitute x v (third i)))
         i)]
    
    [(symbol=? (first i) 'unwrap) i]
    [(symbol=? (first i) 'lt) i]))
 
(check-expect (substitute-instr 'x '(num 1) '(push x))
              '(push (num 1)))
(check-expect (substitute-instr 'x '(num 1) '(lam x ((push x))))
              '(lam x ((push x))))
(check-expect (substitute-instr 'x '(num 1) (list 'push '(pair x (num 5))))
              (list 'push '(pair (num 1) (num 5))))
(check-expect (substitute-instr 'x '(num 1) (list 'add))
              (list 'add))
(check-expect (substitute-instr 'x '(num 1) (list 'sub))
              (list 'sub))
(check-expect (substitute-instr 'x '(num 1) (list 'mul))
              (list 'mul))
(check-expect (substitute-instr 'x '(num 1) (list 'call))
              (list 'call))
(check-expect (substitute-instr 'x '(num 1) (list 'if0 '((push x)) '((push (num 1)))))
              (list
               'if0
               (list (list 'push (list 'num 1)))
               (list (list 'push (list 'num 1)))))
(check-expect (substitute-instr 'x '(num 1) (list 'unwrap))
              (list 'unwrap))
(check-expect (substitute-instr 'x '(num 1) (list 'lt))
              (list 'lt))
 
; substitute-val : Symbol ClosedValue Value -> Value
; substitutes free occurrences of x with v in original value v0
(define (substitute-val x v v0)
  (cond [(symbol? v0)
         (if (symbol=? x v0) v v0)]
        [(symbol=? (first v0) 'num) v0]
        [(symbol=? (first v0) 'thunk) (list 'thunk (substitute x v (second v0)))]
        [(symbol=? (first v0) 'pair)
         (list 'pair (substitute-val x v (second v0)) (substitute-val x v (third v0)))]))
(check-expect (substitute-val 'a '(num 2) 'a) '(num 2))
(check-expect (substitute-val 'a '(num 2) '(num 1)) '(num 1))
(check-expect (substitute-val 'a '(num 2) '(thunk ((push a)))) '(thunk ((push (num 2)))))
(check-expect (substitute-val 'a '(num 2) '(pair a (num 1))) '(pair (num 2) (num 1)))
(check-expect (substitute-val 'a '(num 2) '(pair (num 1) a)) '(pair (num 1) (num 2)))
(check-expect (substitute-val 'a '(num 2) '(pair a a)) '(pair (num 2) (num 2)))

;Exercise 7

; dup : Program
; dup duplicates the top value on the stack,
; transforming a stack from v1,v2,v3... to v1,v1,v2,v3...
(define dup (list '(lam x ((push x) (push x)))))
(check-expect (interp dup '((num 1) (thunk ((push (num 1))))))
              '((num 1) (num 1) (thunk ((push (num 1))))))
(check-expect (interp dup '((thunk ((push (num 1))))))
              '((thunk ((push (num 1)))) (thunk ((push (num 1))))))
 
; drop : Program
; drop drops the top value from the stack,
; transforming a stack from v1,v2,v3... to v2,v3...
(define drop (list '(lam x ())))
(check-expect (interp drop '((num 1) (num 2))) '((num 2)))
(check-expect (interp drop '((thunk ((push (num 1)))))) '())
 
; swap : Program
; swap swaps the top two values on the stack,
; transforming a stack from v1,v2,v3... to v2,v1,v3...
(define swap (list '(lam x ((lam y ((push x) (push y)))))))
(check-expect (interp swap '((num 1) (num 2))) '((num 2) (num 1)))
(check-expect (interp swap '((thunk ((push (num 1)))) (num 2) (num 3)))
              '((num 2) (thunk ((push (num 1)))) (num 3)))


; Exercise 8
; interp-unwrap : Program Stack -> Stack
; pushes the the two values of a pair onto the stack if a pair is the first value in the stack,
; errors otherwise. Then calls the rest of the program and returns the final stack.
(define (interp-unwrap prog stk)
  (if
   (symbol=? (first (first stk)) 'pair)
   (interp (cons (list 'push (third (first stk)))
                 (cons (list 'push (second (first stk)))
                       prog))
           (rest stk))
   (error "Trying to unwrap a non-pair: " (first stk))))

(check-expect (interp-unwrap '() (list '(pair (num 1) (num 2))))
              (list '(num 1) '(num 2)))
(check-expect (interp-unwrap (list '(push (num 8)) '(push (num 5)) '(add))
                             (list '(pair (num 3) (num 4))))
              (list '(num 13) '(num 3) '(num 4)))
(check-expect (interp-unwrap (list '(mul)) (list '(pair (num 3) (num 6)))) (list '(num 18)))
(check-error (interp-unwrap (list '(push 7) '(push 2) '(sub))
                            (list '(thunk (push (num 9))) '(pair (num 5) (num 1))))
             "Trying to unwrap a non-pair: (list 'thunk (list 'push (list 'num 9)))")
;See changes in code as well


; Exercise 9
; diverge : Program
; diverge runs forever when run with an empty stack.
(define diverge '((push (thunk ((lam x ((push x) (push x) (call))))))
                  (lam x ((push x) (push x) (call)))))
;(interp diverge '())

; Exercise 10

(define (interp-lt prog stk)
  (cond
    [(< (length stk) 2) (error "calling lt with fewer than 2 elements in the stack")]
    [(not (equal? (first (first stk)) 'num))
     (error "calling lt when first of stack is not 'num")]
    [(not (equal? (first (second stk)) 'num))
     (error "calling lt when second of stack is not 'num")]
    [else
     (if
      (< (second (first stk)) (second (second stk)))
      (interp prog
              (cons (list 'num 1)
                    (rest (rest stk))))
      (interp prog
              (cons (list 'num 0)
                    (rest (rest stk)))))]))

(check-expect (interp-lt '() '((num 4) (num 3))) '((num 0)))
(check-expect (interp-lt '() '((num 3) (num 4))) '((num 1)))
(check-expect (interp-lt '() '((num 3) (num 3))) '((num 0)))
(check-expect (interp-lt '((sub)) '((num -1) (num 3) (num 1) (num 2))) '((num 0) (num 2)))
(check-error (interp-lt '() '((num 3))) "calling lt with fewer than 2 elements in the stack")
(check-error (interp-lt '() '((num 3) (thunk ((push (num 1))))))
             "calling lt when second of stack is not 'num")
(check-error (interp-lt '() '((thunk ((push (num 1)))) (num 3)))
             "calling lt when first of stack is not 'num")




; An Expression is a:
; - Number
; - Boolean
; - (list AopName Expression Expression)
; - (list BopName Expression Expression)
; - (list CmpopName Expression Expression)
; - (list 'if Expression Expression Expression)
; - (list 'var Symbol)
; - (list 'lam Symbol Type Expression)
; - (list 'app Expression Expression)
 
; compile : Expression -> Program
; translates a Simply Typed Expression into a StackLang Program
(define (compile e)
  (cond [(number? e) `((push (num ,e)))]
        [(boolean? e) `((push (num ,(if e 1 0))))]
        [(member? (first e) AOPS)
         (append (compile (third e))
                 (compile (second e))
                 (list (compile-aop (first e))))]
        [(symbol=? (first e) 'if) (append
                                   (compile (second e)) ; second e is bool
                                   `((if0
                                      ,(compile (fourth e))
                                      ,(compile (third e)))))]
        [(member? (first e) BOPS)
         (append (compile (third e))
                 (compile (second e))
                 (list (compile-bop (first e))))]
        [(member? (first e) CmpOPS)
         (compile-cmop (first e) (second e) (third e))]
        [(symbol=? (first e) 'var) `((push ,(second e)))]
        [(symbol=? (first e) 'lam) `((push
                                      (thunk
                                       ((lam ,(second e)
                                             ,(compile (fourth e)))))))]
        [(symbol=? (first e) 'app) (append (compile (third e))
                                           (compile (second e))
                                           '((call)))]
        [(symbol=? (first e) 'pair) (append
                                     (compile (third e))
                                     (compile (second e))
                                     '((lam x ((lam y ((push (pair x y))))))))]
        [(symbol=? (first e) 'fst) (append
                                    (compile (second e))
                                    '((unwrap)
                                      (lam x ((lam y ((push x)))))))]
        [(symbol=? (first e) 'snd) (append
                                    (compile (second e))
                                    '((unwrap)
                                      (lam x ())))]
        [(symbol=? (first e) 'inleft) (append
                                       (compile (second e))
                                       '((lam x ((push (pair (num 1) x))))))]
        [(symbol=? (first e) 'inright) (append
                                        (compile (second e))
                                        '((lam x ((push (pair (num 0) x))))))]
        [(symbol=? (first e) 'case) (append
                                     (compile (second e))
                                     `((unwrap)
                                       (if0
                                        ,(compile (fourth e))
                                        ,(compile (third e)))
                                       (call)))]))


(check-expect (interp (compile 3) '()) '((num 3)))
(check-expect (interp (compile #t) '()) '((num 1)))
(check-expect (interp (compile #f) '()) '((num 0)))

(check-expect (interp (compile '(lam x Number (var x))) '()) '((thunk ((lam x ((push x)))))))
(check-expect (interp (compile '(app (lam x Number (+ 1 (var x))) 1)) '()) '((num 2)))

(check-expect (interp (compile '(+ 1 2)) '()) '((num 3)))
(check-expect (interp (compile '(- 2 1)) '()) '((num 1)))
(check-expect (interp (compile '(* 2 3)) '()) '((num 6)))

(check-expect (interp (compile '(if #t 3 4)) '()) '((num 3)))
(check-expect (interp (compile '(if #f 3 4)) '()) '((num 4)))

(check-expect (interp (compile '(and 0 0)) '()) '((num 0)))
(check-expect (interp (compile '(and 0 1)) '()) '((num 0)))
(check-expect (interp (compile '(and 1 0)) '()) '((num 0)))
(check-expect (interp (compile '(and 1 1)) '()) '((num 1)))

(check-expect (interp (compile '(or 0 0)) '()) '((num 0)))
(check-expect (interp (compile '(or 0 1)) '()) '((num 1)))
(check-expect (interp (compile '(or 1 0)) '()) '((num 1)))
(check-expect (interp (compile '(or 1 1)) '()) '((num 1)))

(check-expect (interp (compile '(< 1 2)) '()) '((num 1)))
(check-expect (interp (compile '(< 2 1)) '()) '((num 0)))
(check-expect (interp (compile '(< 1 1)) '()) '((num 0)))

(check-expect (interp (compile '(> 1 2)) '()) '((num 0)))
(check-expect (interp (compile '(> 2 1)) '()) '((num 1)))
(check-expect (interp (compile '(> 1 1)) '()) '((num 0)))

(check-expect (interp (compile '(= 1 2)) '()) '((num 0)))
(check-expect (interp (compile '(= 2 1)) '()) '((num 0)))
(check-expect (interp (compile '(= 1 1)) '()) '((num 1)))

(check-expect (interp (compile '(pair 1 3)) '()) '((pair (num 1) (num 3))))
(check-expect (interp (compile '(fst (pair 1 3))) '()) '((num 1)))
(check-expect (interp (compile '(snd (pair 1 3))) '()) '((num 3)))

(check-expect (interp (compile '(inleft 3)) '()) '((pair (num 1) (num 3))))
(check-expect (interp (compile '(inright 3)) '()) '((pair (num 0) (num 3))))
(check-expect (interp (compile '(case (inleft 3) (lam x Number (+ 1 (var x)))
                                  (lam y Number (+ 2 (var y))))) '())
              '((num 4)))
(check-expect (interp (compile '(case (inright 3) (lam x Number (+ 1 (var x)))
                                  (lam y Number (+ 2 (var y))))) '())
              '((num 5)))

(check-expect (interp (compile '(case (inleft (and #t (> 2 3)))
                                  (lam x Number (if (var x) (- 3 4)
                                                    (snd (pair
                                                          (* 3 9)
                                                          (+ 2 -2)))))
                                  (lam y Number (= 1 (fst (pair 3 1)))))) '())
              '((num 0)))
(check-expect (interp (compile '(case (inright (or #f (< 2 3)))
                                  (lam x Number (if (var x) (- 3 4)
                                                    (snd (pair
                                                          (* 3 9)
                                                          (+ 2 -2)))))
                                  (lam y Number (= 1 (fst (pair 3 1)))))) '())
              '((num 0)))
(check-expect (interp (compile '(case (inleft (or #f (< 2 3)))
                                  (lam x Number (if (var x) (- 3 4)
                                                    (snd (pair
                                                          (* 3 9)
                                                          (+ 2 -2)))))
                                  (lam y Number (= 1 (fst (pair 3 1)))))) '())
              '((num -1)))




 
; compile-aop : AopName -> Instruction
; translates an AopName into the corresponding instruction
(define (compile-aop aop)
  (cond [(symbol=? aop '+) '(add)]
        [(symbol=? aop '-) '(sub)]
        [(symbol=? aop '*) '(mul)]))
(check-expect (compile-aop '+) '(add))
(check-expect (compile-aop '-) '(sub))
(check-expect (compile-aop '*) '(mul))

; compile-bop : BopName -> Instruction
; translates a BopName into the corresponding instruction
(define (compile-bop bop)
  (cond [(symbol=? bop 'and) '(if0 ((lam x ((push (num 0)))))
                                   ((if0 ((push (num 0)))
                                         ((push (num 1))))))]
        [(symbol=? bop 'or) '(if0 ((if0 ((push (num 0)))
                                        ((push (num 1)))))
                                  ((lam x ((push (num 1))))))]))
(check-expect (compile-bop 'and) '(if0 ((lam x ((push (num 0)))))
                                       ((if0 ((push (num 0)))
                                             ((push (num 1)))))))
(check-expect (compile-bop 'or) '(if0 ((if0 ((push (num 0)))
                                            ((push (num 1)))))
                                      ((lam x ((push (num 1)))))))

; compile-cmop : CmopName Expression Expression -> Program
; translates a CmopName and proceeding expressions into the corresponding program
(define (compile-cmop cmop fst snd)
  (cond [(symbol=? cmop '<) (append
                             (compile snd)
                             (compile fst)
                             '((lt)))]
        [(symbol=? cmop '>) (append
                             (compile fst)
                             (compile snd)
                             '((lt)))]
        [(symbol=? cmop '=) (append
                             (compile snd)
                             (compile fst)
                             '((sub)
                               (if0 ((push (num 1))) ((push (num 0))))))]))

(check-expect (compile-cmop '< 1 2) '((push (num 2))
                                      (push (num 1))
                                      (lt)))
(check-expect (compile-cmop '> 1 2) '((push (num 1))
                                      (push (num 2))
                                      (lt)))
(check-expect (compile-cmop '= 1 2) '((push (num 2))
                                      (push (num 1))
                                      (sub) (if0 ((push (num 1)))
                                                 ((push (num 0))))))













