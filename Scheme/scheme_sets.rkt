
(define Set-Cardinality length)
;;; car - first element
;;; cdr - second and onwards elememnts
;;; cons construct something with 2 arguments
;;; append - add the elements to the same list
;;; cond - if else kinda ...
(define unique ; sets are non-repeated, must remove duplicates then count
  ( lambda (extra)
     (if(null? extra)
        extra
        (let ((ys (unique (cdr extra)))) ; ys is 2nd element onwards of extra
          (if (member (car extra) ys)
              ys
              (cons (car extra) ys))))))
;;; extra is the list
;;; ys is 2nd element onwards
;;; if first element in extra is in ys, ys is ys
;;; then add car of extra to ys list, repeated theough let call
;;; call as follows (unique (list 1 2 3 4 4 ))
;;; returns (1 2 3 4)

(define set-union
  (lambda (s1 s2)
    (unique (append s1 s2))))
;;; set union as both with the dups removed

;;; intersection of the 2 list as expected input
(define set-intersection
  (lambda (s1 s2)
    (cond ((null? s1) s1)
          ((member (car s1) s2)
           (cons (car s1)
                 (set-intersection (cdr s1) s2)))
          (else (set-intersection (cdr s1) s2)))))

;;; difference og the 2 set inputs
(define set-difference
  (lambda (s1 s2)
    (cond ((null? s1) s1)
          (( member (car s1) s2)
           (set-difference (cdr s1) s2))
           (else (cons (car s1) (set-difference (cdr s1) s2))))))

;;; see if the 2 sets are equal
(define set-equal
  (lambda (s1 s2)
   (and (null? (set-difference s1 s2))
        (null? (set-difference s2 s1)))))
;;; if the diffenece between the both of them is nothing they are the same
;;; (define z 3)
;;; simple definition of things

(define set-map-join
  (lambda (f s)
    (foldl set-union '() (map f s))))