(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else
       (expt (value (car nexp))
             (value (car (cdr (cdr nexp)))))))))

(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+)
       (+ (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '*)
       (* (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp))))))
      (else
       (expt (value2 (car (cdr nexp)))
             (value2 (car (cdr (cdr nexp)))))))))


(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*)
       (* (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
      (else
       (expt (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp)))))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) a)
           (member? a (cdr lat)))))))


(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) #t)
      ((or (number? a1) (number? a2)) #f)
      (else
       (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist (cdr l1) (cdr l2)))))))

;(define equal?
;  (lambda (s1 s2)
;    (cond
;      ((and (atom? s1) (atom? s2))
;       (eqan? s1 s2))
;      ((or (atom? s1) (atom? s2))
;       #f)
;      (else eqlist? s1 s2))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (cdr lat) (car lat)) #f)
      (else
       (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else
       (cons (car lat) (makeset (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat))
       (multirember a (cdr lat)))
      (else
       (cons (car lat) (multirember a (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat)
             (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (l1 l2)
    (cond
      ((null? l1) #t)
      (else
       (and (member? (car l1) l2)
            (subset? (cdr l1) l2))))))

(define eqset?
  (lambda (l1 l2)
      (and (subset l1 l2)
           (subset l2 l1))))

(define intersect?
  (lambda (l1 l2)
    (cond
      ((null? l1) #t)
      (else
       (or (member? (car l1) l2)
           (intersect? (cdr l1) l2))))))


  

