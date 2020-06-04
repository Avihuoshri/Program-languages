 #lang pl 

#| BNF for the MUWAE language:
       <MUWAE> ::= <num>
               | { + <MUWAE> <MUWAE> }
               | { - <MUWAE> <MUWAE> }
               | { * <MUWAE> <MUWAE> }
               | { / <MUWAE> <MUWAE> }
               | { with { <id> <MUWAE> } <MUWAE> }
               | {sqrt <MUWAE> }                             ;;Check
               | <id>
  |#

;; MUWAE abstract syntax trees
(define-type MUWAE
  [Num  (Listof Number)]
  [Add  MUWAE MUWAE]
  [Sub  MUWAE MUWAE]
  [Mul  MUWAE MUWAE]
  [Div  MUWAE MUWAE]
  [Id   Symbol]
  [With Symbol MUWAE MUWAE]
  [Sqrt MUWAE])

;; WAE abstract syntax trees
(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE]
  [SqrtW WAE])


;; parse-sexpr rceives Sexpr as input and returns MUWAE as output
;;this function takes a string  and convert it to MUWAE
;;it wasn't difficult for us to implement the sqrt part
(: parse-sexpr : Sexpr -> MUWAE)
;; to convert s-expressions into MUWAEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num (list n))]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt root_expr) (Sqrt (parse-sexpr root_expr))] 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> MUWAE)
;; parses a string containing a MUWAE expression to a MUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))



#| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
        {sqrt E1}[v/x]        = {sqrt E1[v/x]}                                   
|#


;;subst  function takes 3 arguments as input (MUWAE Symbol MUWAE) and returns MUWAE as output
;;this function was pretty difficult for us to understand so we asked our friends for explenation
;;for the With case and after a few explenations we can say that its claer for us
(: subst : MUWAE Symbol MUWAE -> MUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Sqrt root_expr) (Sqrt (subst root_expr from to))]))                      



#| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
       eval({sqrt E1}) = (sqrt(eval(E1)))                                        
  |#


;;eval receives MUWAE as input and returns Listof Number as output
;;this function responsible for the evaluation from the MUWAE expression part to reducing the expression to numbers part
(: eval : MUWAE -> (Listof Number))
;; evaluates MUWAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (bin-op +  (eval l)  (eval r))]
    [(Sub l r) (bin-op -  (eval l)  (eval r))]
    [(Mul l r) (bin-op *  (eval l)  (eval r))]
    [(Div l r) (bin-op /  (eval l)  (eval r))]
    [(Sqrt e) (sqrt+ (eval e))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (Num  (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    ))



;; sqrt+ receives Listof Number as input and returns Listof Number as output
;; a version of `sqrt' that takes a list of numbers, and return a list  
;; with twice the elements, holding the two roots of each of the inputs
;; this part was very easy and clear and took something like 10 minutes to solve
(: sqrt+ : (Listof Number) -> (Listof Number))  

;; throws an error if any input is negative.
(define (sqrt+ ns)  
  (cond [(null? ns) ns]  
        [(< (first ns) 0) (error 'sqrt+ "there is a negative value in the list , `sqrt' requires a nonnegative input ~s" (first ns) )]
        [else (cons (sqrt  (first ns))
                    (cons  (* (sqrt  (first ns)) -1)  (sqrt+ (rest ns))))]))


(: run : String -> (Listof Number))
;; evaluate a MUWAE program contained in a string
(define (run str)
  (eval (parse str)))




;; applies a binary numeric function on all combinations of numbers from 
;; the two input lists, and return the list of all of the results
;; this function implementation was pretty hard to understand and it took somthing like 1 day to implement
;; after asking for explnations and making a lot of mistakes

(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number)  -> (Listof Number)) 

(define (bin-op op ls rs) 
  (: helper : Number (Listof Number) -> (Listof Number)) 

  (define (helper l rs) 
    (: f : Number -> Number) 
    (define (f num)
      (op l num))
    (map f rs)) 
  (if (null? ls)
      null
      (append (helper (first ls) rs) (bin-op op (rest ls) rs))))  


;; this is the seconed part of the assigment it wasn't so difficult but also not so easy it took a few days to solve (holydays break...)
;; We used one of the TA for the implementation part
;; in this part we were asked to check if there are free instances and return a list of them (if there were any)

;; parse-sexprW receives Sexpr as input and returns WAE as output
;; this function takes a string  and convert it to WAE
(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#


;; same explenation like "subst" function
(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(SqrtW root_expr) (SqrtW (substW root_expr from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
            (substW named-expr from to)
            (if (eq? bound-id from)
                bound-body
                (substW bound-body from to)))]))



;;For this function we used "containsFreeInstance" function that we lerned in TA presentation #6
(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (cases expr
    [(NumW n) null]
    [(AddW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(SubW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(MulW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(DivW l r) (append (freeInstanceList l) (freeInstanceList r))]
    [(SqrtW num) (append (freeInstanceList num))]
    [(WithW bound-id named-expr bound-body)
     (append (freeInstanceList named-expr) 
         (freeInstanceList (substW bound-body
                                       bound-id
                                       (NumW 0))))]

    [(IdW name) (list name)]))







;; TESTS : 

;; run procedure tests : 
(test (run "5") => '(5))
(test (run "{+ 5 5}") => '(10))
(test (run "{* 2 8}") => '(16))
(test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
(test (run "{with {x {/ 5 5}} {* x x}}") => '(1))
(test (run "{with {x {- 6 6}} {* x x}}") => '(0))
(test (run "{with {x {/ 1 1}} {/ x x}}") => '(1))
(test (run "{with {x {/ 1 1}} {sqrt x}}") => '(1 -1))
(test (run "{with {x {+ 4 5}} {sqrt x}}") => '(3 -3))
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run  "{with {x} {with {x {* 1 4}} y}}") =error> "bad `with' syntax in")
(test (run  "{with {x {- 4 {sqrt 9}}} { {* ' {' ' '}}}") =error> "bad syntax in")


;;sqrt procedure tests : : 
(test (run "{sqrt 9}") => '(3 -3))
(test (run "{sqrt 1}") => '(1 -1))
(test (run "{sqrt 16}") => '(4 -4))
(test (run "{sqrt 25}") => '(5 -5))
(test (run "{sqrt 0}") => '(0 0))
(test (run "{sqrt 3}") => '(1.7320508075688772 -1.7320508075688772))
(test (run "{sqrt {with {x {/ 25 5}} {* x x}}}") => '(5 -5))
(test (run "{sqrt {with {x {+ 18 3}} {with {y {- x 3}} {+ y y}}}}") => '(6 -6))
(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative input")  

;;sqrt+ procedure tests : : 
(test (sqrt+ '(4 9 16 25 36)) => '(2 -2 3 -3 4 -4 5 -5 6 -6))
(test (sqrt+ '(49 121 64)) => '(7 -7 11 -11 8 -8))
(test (sqrt+ '(0 0 0 0)) => '(0 0 0 0 0 0 0 0 ))



;; bin-op procedure tests : :

(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{* {sqrt 4} 9}") => '(18 -18))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}")   =>   '(12 -8 11 -9))  
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}")   => '(5 -5 4 -4))


;; freeInstanceList procedure tests :
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 3) (MulW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 3) (DivW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (WithW 'x (NumW 3) (DivW (IdW 'x) (SqrtW(NumW 3))))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{with {x {+ 2 4}} {with {x {/ 4 1}} y}}")) => '(y))
(test (freeInstanceList (parseW "{with {x {* 7 2}} {with {x {- 8 5}} zz}}")) => '(zz))
(test (freeInstanceList (parseW "{with {x {* 3 3}} {with {x {* 3 3}} y}}")) => '(y))
(test (freeInstanceList (parseW "{with {x {- 4 {sqrt 9}}} { {* z {* + y}}}}")) =error> "bad syntax in")
(test (freeInstanceList (parseW "{with {x} {with {x {+ 7 {/ 3 3}}} y}}")) =error> "bad `with' syntax in")




 
