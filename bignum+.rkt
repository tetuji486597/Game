;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bignum+) (read-case-sensitive #t) (teachpacks ((lib "cs17.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "cs17.ss" "installed-teachpacks")) #f)))
(require "bignum-operators.rkt")

; we want to add 729 + 8371 = 9100
; (9 2 7) and (1 7 3 8)
; (bignum+ (list 3 7) (list 7 3 8) result=(list 0)
; (8) (3 8)
; (0 0 1
; (if 
; (if (digit-add (first bignum1) (first bignum2) > 9)
;     (
(define bignum+
  (lambda (bignum1 bignum2 result carry)
    (cond
      [(and (and (empty? bignum1) (empty? bignum2))
            (equal? (first carry) 0)
            result)]
      [(and (and (empty? bignum1) (empty? bignum2))
            (equal? (first carry) 1)
            (cons 1 result))]
      [(empty? bignum1)
       (bignum+ empty
                (rest bignum2)
                (cons (digit-add (first bignum2)
                                 (first carry))
                      result)
                (cons 0 carry))]
      [(empty? bignum2)
       (bignum+ empty
                (rest bignum1)
                (cons (digit-add (first bignum1)
                                 (first carry))
                      result)
                (cons 0 carry))]
      [(> (digit-add (digit-add (first bignum1) (first bignum2))
                     (first carry))
                     9)
             (bignum+  (rest bignum1)
                       (rest bignum2)
                       (cons (digit-sub (digit-add (digit-add (first bignum1)
                                                              (first bignum2))
                                                   (first carry))
                                        10)
                                        result)
                       (cons 1 carry))]
      [true (bignum+ (rest bignum1)
                     (rest bignum2)
                     (cons (digit-add (digit-add (first bignum1)
                                      (first bignum2))
                                      (first carry))
                           result)
                     (cons 0 carry))])))
(bignum+ (list 9 2) (list 1 3) empty empty)

    
    


     