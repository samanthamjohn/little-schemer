(define atom?
  (lambda (x)
    (and (not (pair? x) ) (not (null? x) ) )
  )
)

(define lat?
  (lambda (l)
    (cond
      ( (null? l) true)
      ( (atom? (car l) ) (lat? (cdr l) ) )
      ( else false )
    )
  )
)

(define member?
  (lambda (a lat)
    (cond
      ( (null? lat) false )
      ( (eq? a (car lat) ) true )
      ( else (member? a (cdr lat) ) )
    )
  )
)
(define member*?
  (lambda (a lat)
    (cond
      ( (null? lat) false )
      ( (not (atom? (car lat) ) ) ( or (member*? a (car lat) ) ( member*? a (cdr lat) ) ) )
      ( (eq? a (car lat) ) true )
      ( else (member*? a (cdr lat) ) )
    )
  )
)

(define rember
  (lambda (a lat)
    (cond
      ( (null? lat) () )
      ( (eq? a (car lat) ) (cdr lat) )
      ( else ( cons (car lat) (rember a (cdr lat) ) ) )
    )
  )
)

(define rember
  (lambda (s l)
    (cond
      ( (null? l) () )
      ( (atom? (car l) ) 
       (cond
         ( (and (atom? s) (eq? s (car l) )) (cdr l) )
         ( else (cons (car l) (rember s (cdr l) ) ) )
         )
      ( (equal? (car l) s) 

(define firsts
  (lambda (ls)
    (cond
      ( (null? ls) () )
      ( else ( cons (car (car ls) ) (firsts (cdr ls) ) ) )
    )
  )
)


(define insertr
  (lambda  (new old lat)
    (cond
      ( (null? lat) () )
      ( (eq? (car lat) old) ( cons old (cons new (cdr lat) )  ) )
      ( else (cons (car lat) (insertr new old (cdr lat) ) )  )
    )
  )
)

(define insertr*
  (lambda  (new old lat)
    (cond
      ( (null? lat) () )
      ( (not (atom? (car lat) )) (cons (insertr* new old (car lat) ) (insertr* new old (cdr lat) ) ) )
      ( (eq? (car lat) old) ( cons old (cons new (insertr* new old (cdr lat)) )  ) )
      ( else (cons (car lat) (insertr* new old (cdr lat) ) )  )
    )
  )
)

(define insertl*
  (lambda (new old lat)
    ( cond
      ( (null? lat) () )
      ( (not (atom? (car lat) )) (cons (insertl* new old (car lat) ) (insertl* new old (cdr lat) ) ) )
      ( (eq? (car lat) old) (cons new (cons old (insertl* new old (cdr lat)) ) ) )
      ( else ( cons (car lat) (insertl* new old (cdr lat) )  )  )
    )
  )
)
(define insertl
  (lambda (new old lat)
    ( cond
      ( (null? lat) () )
      ( (eq? (car lat) old) (cons new lat) )
      ( else ( cons (car lat) (insertl new old (cdr lat) )  )  )
    )
  )
)

(define subst
  (lambda (new old lat)
    (cond
      ( (null? lat) () )
      ( (eq? old (car lat) ) (cons new (cdr lat) ) )
      ( else (cons (car lat) (subst new old (cdr lat)) ) )
    )
  )
)

(define subst*
  (lambda (new old lat)
    (cond
      ( (null? lat) () )
      ( (not (atom? (car lat) ) ) (cons (subst* new old (car lat) ) (subst* new old (cdr lat) ) ) )
      ( (eq? old (car lat) ) (cons new (subst* new old (cdr lat)) ) )
      ( else (cons (car lat) (subst* new old (cdr lat)) ) )
    )
  )
)

(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ( (null? lat) () )
      ( (or (eq? old1 (car lat)) (eq? old2 (car lat)) ) (cons new (cdr lat) ) )
      ( else ( cons (car lat) (subst2 new old1 old2 (cdr lat) ) )  )
    )
  )
)

(define multirember
  (lambda (a lat)
    (cond
      ( (null? lat) () )
      ( (eq? (car lat) a) ( multirember a (cdr lat) ))
      ( else ( cons (car lat) (multirember a (cdr lat) ) )  )
    )
  )
)

(define multiInsertR
  (lambda (new old lat)
    (cond
      ( (null? lat) () )
      ( (eq? old (car lat) ) ( cons old (cons new (multiInsertR new old (cdr lat) ) ) ) )
      ( else ( cons (car lat) (multiInsertR new old (cdr lat) ) )  )
    )
  )
)

(define multiInsertL
  (lambda (new old lat)
    (cond
      ( (null? lat) () )
      ( (eq? old (car lat) ) ( cons new (cons old (multiInsertL new old (cdr lat) ) ) ) )
      ( else ( cons (car lat) (multiInsertL new old (cdr lat) ) )  )
    )
  )
)

(define multisubst
  (lambda (new old lat)
    (cond
      ( (null? lat) () )
      ( (eq? old (car lat) ) ( cons new (multisubst new old (cdr lat) ) ) )
      ( else ( cons (car lat) (multisubst new old (cdr lat) ) )  )
    )
  )
)



(define a 'cup)
(define lat '(coffee cup tea cup and hick cup) )

(define new 'fried)
(define old 'fish)
(define lat '(chips and fish or fish and fried))

(define add1 (lambda (n) (+ n 1) )  )
(define sub1 (lambda (n) (- n 1) )  )

(define plus
  (lambda (n m)
    (cond
      ( (zero? m) n )
      ( else (add1 (plus n (sub1 m)) ) )
    )
  )
)

(define minus
  (lambda (n m)
    (cond
      ( (zero? m) n )
      ( else ( sub1 (minus n (sub1 m) ) ) )
    )
  )
)

(define addTup
  (lambda (tup)
    (cond
      ( (null? tup) 0 )
      ( else ( plus (car tup) (addTup (cdr tup) ) ) )
    )
  )
)

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (x n (sub1 m) ) ) )
    )
  )
)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ( (null? tup1) tup2 )
      ( (null? tup2) tup1 )
      ( else (cons (plus (car tup1) (car tup2) ) (tup+ (cdr tup1) (cdr tup2) ) ) )
    )
  )
)

(define gt
  (lambda (n m)
    (cond
      ( (zero? n) false)
      ( (zero? m) true)
      ( else (gt (sub1 n) (sub1 m) ) )
    )
  )
)

(define lt (lambda (n m) (gt m n) ) )

(define my= (lambda (n m) (not (or (gt m n) (lt m n) ) ) ) )

(define pow
  (lambda (n m)
    (cond
      ( (zero? m) 1 )
      ( else (x n (pow n (sub1 m) ) )  )
    )
  )
)

(define div
  (lambda (n m)
    (cond
      ((lt n m) 0 )
      (else (add1 (div (minus n m) m ) ) )
    )
  )
)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat) ) ) )
    )
  )
)

(define pick
  (lambda (a lat)
    (cond
      ((zero? (sub1 n)) (car lat) )
      ( else (pick (sub1 a) (cdr lat) ) )
      )
    )
  )

(define rempick
  (lambda (n lat)
    (cond
      ( (one? n) (cdr lat) )
      ( else (cons (car lat) (rempick (sub1 n) (cdr lat) ) ) )
      )
    )
  )

(define one?  (lambda (n) (= 1 n) ) )

(define no-nums
  (lambda (lat)
    (cond
      ( (null? lat) () )
      ( (number? (car lat) ) (no-nums (cdr lat) ) )
      ( else (cons (car lat) (no-nums (cdr lat) ) ) )
      )
    )
  )

(define all-nums
  (lambda (lat)
    (cond
      ( (null? lat) () )
      ( (number? (car lat) ) (cons (car lat) (all-nums (cdr lat) ) ) )
      ( else (all-nums (cdr lat) ) )
      )
    )
  )

(define eqan
  (lambda (n m)
    (cond
      ( (and (number? n) (number? m) ) (= n m) )
      ( (or (number? n) (number? m) ) false )
      ( else (eq? n m) )
      )
    )
  )

(define occur
  (lambda (a lat)
    (cond
      ( (null? lat) 0 )
      ( (eqan a (car lat) ) (add1 (occur a (cdr lat) )  ) )
      ( else (occur a (cdr lat) )  )
      )
    )
  )
(define occur*
  (lambda (a lat)
    (cond
      ( (null? lat) 0 )
      ( (not (atom? (car lat) ) ) (plus (occur* a (car lat) ) (occur* a (cdr lat) ) ) )
      ( (eqan a (car lat) ) (add1 (occur* a (cdr lat) )  ) )
      ( else (occur* a (cdr lat) )  )
      )
    )
  )

(define rember*
  (lambda (a l)
    (cond
      ( (null? l) () )
      ( (atom? (car l) )
       (cond
        ( (eq? a (car l) ) (rember* a (cdr l) ) )
        ( else ( cons (car l) (rember* a (cdr l) ) ) )
        )
       )
      ( else ( cons (rember* a (car l) ) ( rember* a (cdr l)) ) )
      )
    )
  )

(define leftmost
  (lambda (l)
    (cond
      ( (atom? (car l) ) (car l) )
      ( else (leftmost (car l) ) )
      )
    )
  )

  (define equal?
    (lambda (s1 s2)
      (cond
        ( (and (atom? s1) (atom? s2) ) (eqan s1 s2) )
        ( (or (atom? s1) (atom? s2) ) false)
        (else (eqlist? s1 s2) )
      )
    )
    )

;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ( (and (null? l1) (null? l2) ) true)
;      ( (or (null? l1) (null? l2) ) false)
;      ( (and (atom? (car l1) ) (atom? (car l2) ) ) (and (eqan (car l1) (car l2) ) (eqlist? (cdr l1) (cdr l2)) ) )
;      ( (or (atom? (car l1) ) (atom? (car l2) ) ) false)
;      ( else (and (eqlist? (car l1) (car l2) ) (eqlist? (cdr l1) (cdr l2) ) ) )
;      )
;    )
;  )

(define eqlist?
  (lambda (l1 l2)
  (cond
    ( (and (null? l1) (null? l2) ) true)
    ( (or (null? l1) (null? l2) ) false)
      ( else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2) )) )
    )))
(occur* 'banana '( (banana) (split ((((banana ice))) (cream (banana) ) sherbet) ) (banana) (bread) (banana brandy) ) )
(subst* 'orange 'banana '( (banana) (split ((((banana ice))) (cream (banana) ) sherbet) ) (banana) (bread) (banana brandy) ) )
(member*? 'orange '( (banana) (split ((((banana ice))) (cream (banana) ) sherbet) ) (banana) (bread) (banana brandy) ) )
(member*? 'banana '( (banana) (split ((((banana ice))) (cream (banana) ) sherbet) ) (banana) (bread) (banana brandy) ) )
(leftmost '( (banana) (split ((((banana ice))) (cream (banana) ) sherbet) ) (banana) (bread) (banana brandy) ) )
(leftmost '( fruit (banana) (split ((((banana ice))) (cream (banana) ) sherbet) ) (banana) (bread) (banana brandy) ) )

(eqlist? '(beef (sausage (and) (soda) ) ) `(beef (sausage (and) soda) ) )
(eqlist? '(beef (sausage (and) (soda) ) ) `(beef (sausage (and) (soda)) ) )
(eqlist? '(beef (sausage (and) (soda) ) ) `(beef (sausage (and) (pop)) ) )
(equal? '(beef (sausage (and) (soda) ) ) 3)
(equal? '(beef (sausage (and) (soda) ) ) `foo)
(equal? 2 3)
(equal? 2 2)
(equal? 2 `foo)
(equal? `foo `foo)
