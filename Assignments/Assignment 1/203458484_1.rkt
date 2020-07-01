#lang pl



;                                                         --------------------Question 1 --------------------

#|
Functions signatures
|#

(: plSuffixContained : (Listof String) ->(U String Boolean) )
(: checkWord : String -> Boolean )


#|
plSuffixContained function receive a list of strings and check if it contains a word with a suffix that equals to "pl"
if it does the function returns the word else it returnes #false
|#
(define (plSuffixContained list)
  (if (null? list) #false
      (if (= (string-length(first list)) 1) (plSuffixContained (rest list))
                             (if (checkWord (first list ))(first list) (plSuffixContained (rest list)) ))
      )) 


#|
checkWord function receive a word  and check if it contains a suffix that equals to "pl"
if it does the function returns #true else it returnes #false
|#
(define (checkWord str)

(if (string=? str "") #false
    (if(string=? (substring str (- (string-length str ) 2) (string-length str )) "pl"
     )
   #true #false
   ))
  
)


#|--------------- checkWord function tests ---------------|#

(test (checkWord "applepl") => #true)
(test (checkWord "pl") => #true)   
(test (checkWord "") => #false)    #|empty string|#
(test (checkWord "abaplab") => #false)




#|--------------- plSuffixContained function tests ---------------|#

(test (plSuffixContained  '("hello" "apple" "pl")) => "pl")
(test (plSuffixContained  '("" "apple" "pl")) => "pl")
(test (plSuffixContained  '("hello" "apple" )) => #false)
(test (plSuffixContained  '("" "" "")) => #false)
(test (plSuffixContained  '("33" "123" "2")) => #false)





;                                                         --------------------Question 2.1 --------------------



#|Functions signatures|#
(: write-poly : (Listof Integer) -> String)
(: write-poly-tail : (Listof Integer) Integer Integer -> String)
(: build-poly-string : Number  Integer Integer -> String)
(: is-all-zero : (Listof Number) -> Boolean)

#|Defining the "write-poly" function implemetation : this functions receive list of Integers and outputs a String
  that represents a polynom.
  it also uses "help functions" such as write-poly-tail , is-all-zero
|#
(define ( write-poly list)
  (if (null? list) ""
      (if (is-all-zero list) "0" (if (equal? (string (string-ref(write-poly-tail list (- (length list) 1) (length list) ) 0)) "+" )
      (substring
       (write-poly-tail list (- (length list) 1) (length list)) 1
       (string-length (write-poly-tail list (- (length list) 1) (length list))))
      (write-poly-tail list (- (length list) 1) (length list))))
  )
  
   )

#|Defining the "write-poly-tale" function implemetation : this functions receive 3 arguments:
 1.list of Integers
 2.Integers that represents the counter
 3.the original list length
 and outputs a String  that represents a polynom
 it also uses "help functions" such as build-poly-string
|#
( define (write-poly-tail list counter list_length)
   (if (null? list) "" (string-append (build-poly-string(first list) counter list_length) (write-poly-tail (rest list) (- counter 1) list_length))))
   

#|Defining the "write-poly-tale" function implemetation :the main goal of this function is to build a polinomial value as a String
 this functions receive 3 arguments:
 1.list of Integers
 2.Integers that represents the counter
 3.the original list length
 and outputs a String  that represents a polynom
|#
(define (build-poly-string num counter list_length)
 (cond
   [(= num 0) "" ]
   [(and (> num 1 ) (= counter 0)) (string-append "+"(number->string num))]
   [(and (< num 0 ) (= counter 0)) (number->string num)]
   [(and (< num 0 ) (= counter 1)) (string-append (number->string num) "x")]
   [(and (= num -1 ) (> counter 1)) (string-append  "-1x^" (number->string counter))]
   [(and (< num 0 ) (> counter 1)) (string-append (number->string num) (string-append"x^" (number->string counter)))]
   [(and (> num 1 ) (and (> counter 1) (= counter (- list_length 1 )))) (string-append (number->string num) (string-append"x^" (number->string counter))) ]
   [(and (> num 1 ) (> counter 1)) (string-append (string-append "+"(number->string num)) (string-append"x^" (number->string counter))) ]
   [(and (> num 1 ) (= counter 1)) (string-append (string-append "+"(number->string num)) "x" ) ]
   [(and (= num 1 ) (> counter 1)) (string-append "+"  (string-append"1x^" (number->string counter))) ]
   [(and (= num 1 ) (= counter 1)) (string-append "+" "1x") ]
   [(and (= num 1 ) (= counter 0)) (string-append "+" "1")]
   [else "error"])
  )

#|Defining the "is-all-zero" function implemetation : this function receive a list and check if all the list's
  values are 0
|#
(define (is-all-zero list)
  (if(null? list ) #true  (if(= (first list) 0 ) (and (is-all-zero (rest list)) #true) #false))
 )


#|Tests |#
(test(is-all-zero '(0 0 0)) => #true)
(test(is-all-zero '(0 3 0)) => #false)

(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(0 3 1)) => "3x+1")
(test (write-poly '(0 1 1)) => "1x+1")
(test (write-poly '(1 1 1)) => "1x^2+1x+1")
(test (write-poly '(-1 1 1)) => "-1x^2+1x+1")
(test (write-poly '(-1 1 -1)) => "-1x^2+1x-1")
(test (write-poly '(0 0 -1)) => "-1")
(test (write-poly '(0 -7 0)) => "-7x")
(test (write-poly '(-3 1 2)) => "-3x^2+1x+2")
(test (write-poly '(0 0 0)) => "0")


;                                                         --------------------Question 2.2 --------------------




#|Functions signatures|#
(:  compute-poly : Number (Listof Number) ->  Number)
(:  calculate-x :  Number  Number ->  Number)

#||#
(define (compute-poly x list)
   (if (null? list) 0
        (+ (* (first list) (calculate-x x (- (length list) 1))) (compute-poly x (rest list))))
        )


(define (calculate-x  x power)
  (if( = power  0 ) 1
      (if ( = power 1 ) x
                        (* x (calculate-x  x (- power 1))))
      ))

(test (compute-poly  2 '(1 2 -3 -1) ) => 9)
(test (compute-poly  0 '() ) => 0)
(test (compute-poly -9 '(1 1 1 1) ) => -656 )
(test (compute-poly  0.5 '(1 0 -3) ) => -2.75)
(test (compute-poly  2 '(1 2 5 -6 -8 6 -1 4 -0.5 0.25) ) => 1127.25 )
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)





;                                                         --------------------Question 3 --------------------



#|Solution for qustion 3_1 and 3_2|#
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

#|Solution for qustion 3_3
"search-stack" operation receive two arguments (Symbol and KeyStack)
and return false if the stack is empty or returns value that is keyed accordingly  |#
(: search-stack : Symbol KeyStack -> (U String #false))
(define (search-stack  symbol keyStack)
  (cases keyStack
    [(EmptyKS) #false]
    [(Push sym str stack) (if (equal? sym symbol) str (search-stack sym stack))])
  )

#|Solution for qustion 3_4
"pop-stack operation receive a stack (KeyStack) and return the first value (LIFO)
 if the stack is empty it returns false|#
(: pop-stack : KeyStack -> (U KeyStack #false))
(define (pop-stack keystack)
  (cases keystack
    [(EmptyKS) #false]
    [(Push sym str stack) stack]))

#|Tests|#
(test (EmptyKS) => (EmptyKS))

(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>(Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))

(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")

(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)

(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (pop-stack (EmptyKS)) => #f)






;                                                         --------------------Question 4 --------------------





(: is-odd? : Natural -> Boolean)

;; Function signature : "is-odd" function receive as input a Natural number (>= 0) and
;; outputs a Boolean value (#true / #false ).
;; this function check if a number is an odd number and if it does the function outputs #true
;; it uses "is-even?" function that substract 1 fron the given number until it equals to zero
;; and output boolean value depends wich function it stopped 
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)

;;Function signature : :"is-even?" function receive a Natural number and outputs a Boolean value
;; it uses "is-odd?" function that substract 1 fron the number that "is-even?" recived and if it
;;equals to zero it means the original number was odd else it was even

(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
;; function every? receive as input 2 values : first value is a function that receive type A (any kind) and outputs Boolean
;; and the seconed value is a function that receive  list of type A and outputs Boolean)
;; the function every outputs Boolean.
;; This function checks if a list contain a value (of type A) and return true if its exists else return false 
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)

;; all even function receive as input list of Natural numbers and return as output Boolean value
;; The function checks if all the numbers in the list are even using sub functions "every?" and "is-even" 
(define (all-even? lst)
 (every?
  is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))

;; every2? receive as input 4 values of type A and B (A , B an be the same type or different types) and outputs Boolean value
;; This function does a paralel check if all the values in lst1 equals to pred1
;; and if all the values in lst2 equals to pred2
;; if it does the function returns true else returns false

(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))