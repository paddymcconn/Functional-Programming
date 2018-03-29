#lang racket
(define (reduce f)                                                          ; 1
  ((lambda (value) (if (equal? value f) f (reduce value)))                  ; 2
   (let r ((f f) (g '()))                                                   ; 3
     (cond ((not (pair? f))                                                 ; 4
	    (if (null? g) f (if (eq? f (car g)) (cadr g) (r f (caddr g))))) ; 5
	   ((and (pair? (car f)) (= 2 (length f)) (eq? 'lambda (caar f)))   ; 6
	    (r (caddar f) (list (cadar f) (r (cadr f) g) g)))               ; 7
	   ((and (not (null? g)) (= 3 (length f)) (eq? 'lambda (car f)))    ; 8
	    (cons 'lambda (r (cdr f) (list (cadr f) (delay (cadr f)) g))))  ; 9
	   (else (map (lambda (x) (r x g)) f))))))                          ;10