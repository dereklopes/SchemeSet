(define (empty? s)
  (null? s)
)

(define (set s)
  (cond ((empty? s) s)
        ((in? (car s) (cdr s)) (set (cons (car s) (discard (car s) (cdr s)))))
        (else (cons (car s) (set (cdr s))))) ; not tail recursive
)

(define (in? e s)
  (cond ((empty? s) #f)
        ((eqv? e (car s)) #t)
        (else (in? e (cdr s))))
)

(define (add e s)
  (if (in? e s)
      s
      (cons e s))
)

(define (discard e s)
  (cond ((empty? s) s)
        ((eqv? e (car s)) (discard e (cdr s)))
        (else (cons (car s) (discard e (cdr s))))) ; not tail recursive
)       

(define (union s1 s2)
  (cond ((empty? s1) s2)
        ((in? (car s1) s2) (union (cdr s1) s2))
        (else (union (cdr s1) (add (car s1) s2))))
)

(define (intersection s1 s2)
  (cond ((empty? s1) '())
        ((in? (car s1) s2) (add (car s1) (intersection (cdr s1) s2))) ; not tail recursive
        (else (intersection (cdr s1) s2)))
)

(define (difference s1 s2)
  (cond ((empty? s1) '())
        ((in? (car s1) s2) (difference (cdr s1) s2))
        (else (add (car s1) (difference (cdr s1) s2)))) ; not tail recursive
)

(define (symmetric-difference s1 s2)
  (cond ((empty? s1) s2)
        ((empty? s2) s1)
        ((in? (car s1) s2) (symmetric-difference (cdr s1) (discard (car s1) s2)))
        (else (cons (car s1) (symmetric-difference (cdr s1) s2)))) ; not tail recursive
)

(define (subset? s1 s2)
  (cond ((empty? s1) #t)
        ((empty? s2) #f)
        ((in? (car s1) s2) (subset? (cdr s1) s2))
        (else #f))
)
          
(define (superset? s1 s2)
  (cond ((empty? s1) #f)
        ((empty? s2) #t)
        ((in? (car s2) s1) (superset? s1 (cdr s2)))
        (else #f))
)

(define (disjoint? s1 s2)
  (cond ((and (empty? s1) (empty? s2)) #f)
        ((empty? s1) #t)
        ((empty? s2) #t)
        ((in? (car s1) s2) #f)
        (else (disjoint? (cdr s1) s2)))
)

(define (sameset? s1 s2)
  (cond ((and (empty? s1) (empty? s2) #t))
        ((empty? s1) #f)
        ((empty? s2) #f)
        ((in? (car s1) s2) (sameset? (cdr s1) (discard (car s1) s2)))
        (else #f))
)


; some tests
(define A (set '(1 2 7 9 7 1)))
(define B (set '(2 0 8 0 7 12)))
(define C (set '(9 7)))

(define colors (set '("yellow" "red" "green" "blue" "orange" "purple" "pink")))
(define rgb (set '("red" "green" "blue")))

(define hi (set '(#\h #\i)))

(empty? A) ; #f
(empty? rgb) ;#f
(empty? (set'())) ;#t

(in? 0 A) ; #f
(in? "red" A); #f
(in? 2 A) ; #t

(in? "green" rgb) ; #t
(in? "purple" rgb) ; #f
(in? "i" hi) ;#f
(in? #\i hi) ;#t

(add 9 A) ; (2 9 7 1)
(add 5 A) ; (5 2 9 7 1)

(discard 1 A) ; (2 9 7)
(discard 5 A) ; (2 9 7 1)
(union A B) ; (9 1 2 8 0 7 12)
(union A rgb) ; (2 9 7 1 "red" "green" "blue")

(intersection A rgb) ; ()
(intersection A B) ; (2 7)
(intersection rgb colors) ; ("red" "green" "blue")

(difference A B) ; (9 1)
(difference rgb colors) ; ()
(difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(symmetric-difference A B) ; (9 1 8 0 12)
(symmetric-difference A C) ; (2 1)
(symmetric-difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(subset? A B) ;#f
(subset? C A) ; #t

(subset? colors rgb) ;#f
(subset? rgb colors)  ; #t

(superset? A B) ;#f
(superset?  A C) ; #t

(superset? colors rgb) ;#t
(superset? rgb colors)  ; #f

(disjoint? B C) ;#f
(disjoint? colors A) ;#t

(sameset? (set '(9 1 2 7)) A); #t
(sameset? B A) ; #f