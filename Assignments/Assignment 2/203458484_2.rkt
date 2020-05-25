
; The ROL BNF and Parsing code:
#lang pl
;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter
#| BNF for the RegE language:
 <ROL>  ::= {reg-len =  <num> <RegE>}
   <RegE> ::= {And <RegE> <RegE> } | {Or <RegE> <RegE>} | {Shl <RegE>} | {<Bits>}
   <Bits> ::= 1 | 0 | 1<Bits> | 0<Bits>
|#

#|Question 1.1_b|#
;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE])


#|

Question 1.1_c
Examples of 3 ROL words :

FIRST EXAMPLE :  " {reg-len = 8  {1 0 1 1 0 0 0 1}} "
<ROL>
 => {reg-len =  <num> {<RegE>}}
             => {reg-len = 8 {<RegE>}}
             => {reg-len = 8 {<Bits>}}
             => {reg-len = 8  {1<Bits>}}
             => {reg-len = 8  {1 0<Bits>}}
             => {reg-len = 8  {1 0 1<Bits>}}
             => {reg-len = 8  {1 0 1 1<Bits>}}
             => {reg-len = 8  {1 0 1 1 0<Bits>}}
             => {reg-len = 8  {1 0 1 1 0 0<Bits>}}
             => {reg-len = 8  {1 0 1 1 0 0 0<Bits>}}
             => {reg-len = 8  {1 0 1 1 0 0 0 1}}


                   <ROL>
        _____________|____________________________________
       |   {reg-len =  <num>                 {<RegE>}}   |
                       __|__                 ____|_____
                      |  8  |               |  <Bits>  |
                                           ______|__________
                                          |    1<Bits>      |
                                             ______|__________
                                            |    1 0<Bits>    |
                                                ______|____________
                                               |    1 0 1<Bits>    |
                                                     ______|_____________
                                                    |    1 0 1 1<Bits>   |
                                                            ______|_______________
                                                           |    1 0 1 1 0<Bits>   |
                                                                     ______|_______________
                                                                    |    1 0 1 1 0 0<Bits> |
                                                                                ______|___________________
                                                                               |    1 0 1 1 0 0 0<Bits>   |
                                                                                             ______|_______________
                                                                                            |   1 0 1 1 0 0 0 1    |






SECONED EXAMPLE :     "{reg-len 2 {or {0 0} {1 1}}}"
<ROL> =>
      => {reg-len =  <num> {<RegE>}}
      => {reg-len =  2 {<RegE>}}
      => {reg-len =  2 {Or{<RegE>}{<RegE>}}}
      => {reg-len =  2 {Or{<Bits>}{<Bits}}}
      => {reg-len =  2 {Or{0<Bits>}{<Bits}}}
      => {reg-len =  2 {Or{0 0}{<Bits}}}
      => {reg-len =  2 {Or{0 0}{1<Bits}}}
      => {reg-len =  2 {Or{0 0}{1 1}}
     


                    <ROL>
        _____________|____________________________________
       |   {reg-len =  <num>                 {<RegE>}}   |
                       __|__                 ____|____________________________________________________________
                      |  2  |              |   {Or{<RegE>}                                       {<RegE>}}    |
                                                ______|____                                      ____|_____
                                               |  <Bits>   |                                    |  <Bits>  |  
                                                ______|____                                      ____|_____
                                               |  0<Bits>   |                                   |  <Bits>  |  
                                                   __|__                                        ____|_____
                                                  | 0 0 |                                      |  <Bits>  |
                                                                                                 _____|_____
                                                                                                |  1<Bits>  |  
                                                                                                   ___|___
                                                                                                  |  1 1  |  




THIRD EXAMPLE :  " {reg-len 2 {or {shl {1 0}}{shl {0 1}}}} ->  FAST DERIVATION

<ROL>  =>
       =>{reg-len <num> <RegE>}
       =>{reg-len 2 {or <RegE> <RegE>}}
       =>{reg-len 2 {or {Shl <RegE>} {Shl <RegE>}}}
       =>{reg-len 2 {or {Shl {<Bits>}} {Shl {<Bits>}}}}
       =>{reg-len 2 {or {Shl {1<Bits>}} {Shl {0<Bits>}}}}
       =>{reg-len 2 {or {Shl {1 0}} {Shl {0 1}}}}




                    <ROL>
         _____________|____________________________________
         |   {reg-len =  <num>                 {<RegE>}}   |
                         __|__                 ____|____________________________________________________________
                        |  2  |              |   {Or <RegE>                                       <RegE>}       |
                                                 ______|_________                                 __|_____________
                                                | {Shl <RegE>}   |                               | {Shl <RegE>}   |
                                                     _____|____                                      _____|_____
                                                    | {<Bits>} |                                     | {<Bits>} |
                                                     _____|____                                      _____|_____
                                                    |  1<Bits> |                                    |  0<Bits>  |
                                                        __|__                                            __|__
                                                       | 1 0 |                                          | 0 1 |


|#



;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond
    [(null? lst) null]
    [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
    [else (cons 0 (list->bit-list (rest lst)))]))


(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr 
    [(list 'reg-len '= (number: num) reList)
     (if(> num 0) (parse-sexpr-RegL  reList num) (error 'parse-sexpr "register is empty error , size should be at least  1 ~s" sexpr) )]
    ;; remember to make sure specified register length is at least 1
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... )
     (cond
       [(= reg-len(length a))(Reg(list->bit-list a))]
       [else(error 'parse-sexpr "wrong number of bits in ~s" a)])]  
    [(list 'and left right)(And (parse-sexpr-RegL left reg-len) (parse-sexpr-RegL right reg-len))]
    [(list 'or left right)(Or (parse-sexpr-RegL left reg-len) (parse-sexpr-RegL right reg-len))]
    [(list 'shl expr)(Shl (parse-sexpr-RegL expr reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: sum-of-squares : (Listof Number) -> Number )


;; tests
(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl(Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 0 {2 3}}") =error> "register is empty error , size should be at least  1")
(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse"{ reg-len = 3 {or {2 2 1} {0 1 1}}}" ) =error>  "bad syntax in")
(test (parse "{ reg-len = 2 {+ {1 1 1} {0 1 1}}}"  ) =error> "bad syntax in")






#|Question 2_1_a
  My explnation for this problem is that the calculating order is

  VERY IMPORTANT because there are several calculation answers for example :

  * if {set 2} was the last assignment , so the answer will be 6 => (1+2)*2

  * if {set 1} was the last assignment , so the answer will be 3 => (1+2)*1

  this problem occur when there is ambiguity .

  Question 2_!_b

  To solve the abiguity problem I need to define <MAE> BNF tree without ambiguity .
  To do that I will need to use the <AE> BNF tree for expressions like {set {+ 2 3 }: 


<MAE> ::=   <AE>             ;; *
            |{ set <MAE> }         ;; {set {+ 8 7} }-> { set <AE> } - as described in the example
            |{+ set <MAE> <MAE>}
            |{- set <MAE> <MAE>}
            |{* set <MAE> <MAE>}
            |{/ set <MAE> <MAE>}
            |{+ <MAE> get}             
            |{- <MAE> get}
            |{* <MAE> get}          ;; {* get get} - as described in the example
            |{/ <MAE> get}
            |get

 * I took the <AE> BNF tree from the from the moodlearn -> "Evaluetion lecture summery" :
      <AE> ::= <num>            
            | { + <AE> <AE> }
            | { - <AE> <AE> }
            | { * <AE> <AE> }
            | { / <AE> <AE> }


      <num> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

  Now we can see that there is an operation order because there are curly brackets that
  separets each operation that makes the <MAE> BNF tree to be unambigous


-------------------------------------------------------------------------------------------

Question 2.2

For this question I will use the <AE> that I used last question

<MAE> ::= {seq {<AE>}}     ;; For arithmetic expression like {seq {- 8 2}}

             |{seq {<SETE>}}  ;; For expressions that starts with 'set' expressions -> SETE = Set Expression


<SETE> ::= <AE_GET>
          | {set <AE>}  <SETE>  ;;  {set {+ 8 {* 6 5}}
          | {set <AE_GET>} <SETE>




<AE_GET> ::=
             get 
            | <num>
            |{ + <AE_GET> <AE_GET> }
            |{ - <AE_GET> <AE_GET> }
            |{ * <AE_GET> <AE_GET> }    ;;for example :  {* get get} or {* {* get get} {* get get}}
            |{ / <AE_GET> <AE_GET> }




|#








#|Question 3
 sum-of-squars receive as input list of Numbers (positive and/or negative) and
 return as input a Number value
 it uses foldl function to activate numPower function that receive as input a Number and return as output the same Number but powerd by 2
 for every value in the list sum all the values powered by 2
 in the process it sums up al the result and return the answer
|#

(: sum-of-squares : (Listof Number) -> Number )
(: numPower : Number -> Number)


(define (sum-of-squares lst)
  ( foldl + 0 (map numPower lst)))

(define (numPower number)
  (* number number))


(test (sum-of-squares '()) => 0)



(test (sum-of-squares '( 1 2 )) => 5)
(test (sum-of-squares '( 0 0  )) => 0)
(test (sum-of-squares '( 0 0  1 )) => 1)
(test (sum-of-squares '( 0 -2  1 -3 0.5 )) => 14.25)
(test (sum-of-squares '( 0 -3 -5 -10 -2 -1  )) => 139)
(test (numPower 0) => 0)
(test (numPower 2) => 4)
(test (numPower -2) => 4)



#|Question 3  summary :
This question was preatty easy for me after I understand wht to do
|#



#|In this question I defined a type called BINTREE that consists Node and Leaf |#
;;Question 4_a
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

;;Question 4_b 
#|"tree-map" function receive as input a numeric function and BINTREE
  and outputs a BINTREE.
  the "tree-map" function use the input function on each node of the tree for example
  if we use the function sub1 on each node of the given binary tree we will receive as input
  the same tree stracture but each node value will be substracted by 1|#

(: tree-map : (Number -> Number) BINTREE -> BINTREE )

(define (tree-map f binary_tree)
  (cases binary_tree
    [(Leaf leaf_value)  (Leaf(f leaf_value))] ;; Case 1 : node without children (leaf) so activate function on the value
    [(Node left_child right_child) (Node(tree-map f left_child) (tree-map f right_child))]) ) ;; Case 2 : node with children so activate for each child tree-map function in recurtion



;;Question 4_c
#|Tests for question 4_b|#
(test (tree-map add1 (Node (Leaf 0)(Leaf 1))) => (Node (Leaf 1) (Leaf 2)))
(test (tree-map sub1 (Node (Leaf 0)(Leaf 1))) => (Node (Leaf -1) (Leaf 0)))

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))

(test (tree-map add1 (Node (Node (Leaf 1.5) (Leaf 2.3)) (Node (Node (Leaf 8.6) (Leaf 10.2)) (Leaf 6))))
      => (Node (Node (Leaf 2.5) (Leaf 3.3)) (Node (Node (Leaf 9.6) (Leaf 11.2)) (Leaf 7))))
(test (tree-map sub1 (Node (Node (Leaf 0) (Leaf 3)) (Node (Node (Leaf 8) (Leaf 10)) (Leaf 6))))
      => (Node (Node (Leaf -1) (Leaf 2)) (Node (Node (Leaf 7) (Leaf 9)) (Leaf 5))))



;;Question 4_d
#|tree-fold is a template like unction that receive 3 arguments
  first argument : a function that receive 2 A type arguments and return A type argument
  seconed argument : a function that receive Number tpe argument and return A type argument
  third argument : BINTREE - binary tree

  the tree-fold function use a function to combine all the tree nodes depends what the function does
  for example if tree-fold receive the function 'append' and a type 'list' it makes a list from all the leafs
|#
(: tree-fold : (All(A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold  node_f leaf_f bin_tree)
  (cases bin_tree
    [(Leaf leaf_value) (leaf_f leaf_value)]
    [(Node  left_child right_child)
     (node_f (tree-fold  node_f leaf_f left_child ) (tree-fold node_f leaf_f right_child))]))



;;Question 4_e
#|tree-flatten function receive as input a BINTREE and
  returns as output a list of its values from left to right as described in the homework document
|#
(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number) tree))

#|I used these tests to check tree-flatten and tree-fold function at the same time|#
(test (tree-flatten (Node (Leaf 1)(Leaf 2))) => '(1 2))
(test (tree-flatten (Node (Leaf 0.5)(Leaf 0.25))) => '(0.5 0.25))
(test (tree-flatten (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))) => '(1 2 3 4))
(test (tree-flatten (Node (Node (Leaf -1) (Leaf 2)) (Node (Leaf -3) (Leaf 4)))) => '(-1 2 -3 4))
(test (tree-flatten (Node (Node (Node (Leaf 1)(Leaf 2))(Node (Leaf 3)(Leaf 4)))(Node (Leaf 5)(Leaf 6)))) => '(1 2 3 4 5 6))

;;Question 4_g
#|tree-reverse function receive as input a binary tree and returns the same tree but in a
reverse order by using "tree fold" "switch-nodes" functions
the  use of tree-fold function is for moving  and activating "switch-nodes function
on each node of the tree (left node becomes right node and right node becomes left node like mirror)|#
(: tree-reverse : BINTREE -> BINTREE)
(: switch-nodes : BINTREE BINTREE -> BINTREE)

(define (tree-reverse node)
  (tree-fold switch-nodes Leaf node))

;; This function is for swapping left BINTREE with the right BINTREE
(define (switch-nodes leftNode rightNode)  
  (Node rightNode leftNode) )

#|Test|#

(test ( tree-reverse ( Node ( Leaf 1 )( Leaf 2 ))) => ( Node ( Leaf 2 ) ( Leaf 1 )))
(test ( tree-reverse ( Leaf 1 )) => (Leaf 1) )
(test ( tree-reverse ( Node ( Leaf -2 )( Leaf -1 ))) => ( Node ( Leaf -1 ) ( Leaf -2 )))
(test ( tree-reverse ( Node ( Leaf 0.51 )( Leaf 0.25 ))) => ( Node ( Leaf 0.25 ) ( Leaf 0.51 )))
(test ( tree-reverse ( Node (Node ( Leaf 1 )( Leaf 2 )) ( Node ( Leaf 3) ( Leaf 4 )))) => ( Node (Node ( Leaf 4 )( Leaf 3 )) ( Node ( Leaf 2) ( Leaf 1 ))))


#|Question 4  summary :
  It wasn't so easy for me to solve this question and I asked for help and explanations from the lecturer , from the T.A ,
  and also a couple of friends about tree-fold tests because I wanted to check my function
|#






