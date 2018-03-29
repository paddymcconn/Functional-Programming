;;; merges 2 lists
 (define merge
   (lambda (x y) ; annon function
     (append x y) ; add x to y
         (cond((null? x) y) ;; condition if x is null return y
                (else (cons (car x) (append (cdr x) y)))))) ; otherwise, construct first of x with the rest of x and then y

;;; deletes all occurances of a selected item in a list
(define deleteall
  (lambda (x y)
      (if (null? y)
          y
      (if (eqv? (car y) x)
           (deleteall x (cdr y))
      (cons (car y) (deleteall x (cdr y)))))))

;;; this is used for deleting the nth occurances of an item in a list, eg (ndelete 2 'a '(a b a c a)) becomes (a b c a) 
(define z 1)
(define (ndelete n x y)
       (cond ((null? y) '())
              ((eqv? x (car y)) (cond ((< z n) (set! z (+ z 1)) (cons (car y) (ndelete n x (cdr y))))
                      (else (set! z 1) (ndelete n x (cdr y)))))
              (else (cons (car y) (ndelete n x (cdr y))))))

;;; sees if two things are equal
  (define equal(lambda (x y)
      (cond((and(null? x) (null? y)) #t)
            ((and(not (null? x)) (not (null? y))) (equal (cdr x) (cdr y)))
            (else #f))))

;;; copies from one list to another, could be useful.
(define (copy x)
       (cond((null? x) '())
               (else (cons (car x) (copy(cdr x)) ))))

(define palindrome? 
   (lambda (xs) 
     (equal? xs (reverse xs))))

 (define dupli 
   (lambda (xs) 
     (if (null? xs) '() 
       (cons (car xs) 
             (cons (car xs) (dupli (cdr xs)))))))

; 2015 Autumn
(define reverse-with-count
  (lambda (xs nums)
    (cond ((null? xs) '())

          (else
           (append (reverse-with-count (cdr xs) (cdr nums)) (repeat (car xs) (car nums)))))))

(define repeat (lambda (letter times)
                 (cond ((zero? times) '())
                       
                        (else
                        (cons letter (repeat letter (- times 1)))))))

; (reverse-with-count '(a b c) '(1 2 3))

(define repeat-n-times
  (lambda (x)
   ; print "Hello World"
    (if (> x 0)
        (print "Hello World")
      (repeat (- x 1)))))

(define arr-repeat-n-times
  (lambda ( x y)
    (if (null? y)
        '()
        (append (make-list x (car y)) (arr-repeat x (cdr y))))))

(define add
  (lambda (x y)
    (+ x y)))

(define repeat
  (lambda (x)

    (if ((number? x))
        (display (+ x 1)))
    (repeat (- x 1))))
