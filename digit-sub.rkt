;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname digit-sub) (read-case-sensitive #t) (teachpacks ((lib "cs17.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "cs17.ss" "installed-teachpacks")) #f)))
; digit-sub.rkt

; digit-sub: list -> list

; Input: two list of numbers, bignum1 and bignum2, representing bignums
; Output: a list of numbers representing bignum1-bignum2

(define digit-add
   (lambda (a b)
     (if (and ( integer? a )
              ( integer? b )
              ( <= 0 a )
              ( <= a 99)
              ( <= 0 b )
              ( <= b 99)
              ( >= (- a b) 0))
         (- a b)
         (error ' digit-add " inputs or output out of range. tried (+ ~a ~a) "
      a b))))