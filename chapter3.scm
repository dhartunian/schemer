(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) 
		 (rember a (cdr lat)))))))

(define firsts
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car (car lat))
		 (firsts (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons (car lat) 
				(cons new (cdr lat))))
     (else (cons (car lat)
		 (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new 
				(cons old (cdr lat))))
     (else (cons (car lat)
		 (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat)
		 (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1) (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat)
		 (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat))
      (multirember (cdr lat)))
     (else (cons (car lat)
		 (multirember a lat))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons old (cons new
		      (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
	  (cons new (cons old
			  (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat))
      (cons new
	    (multisubst new old (cdr lat))))
     (else (cons (car lat)
		 (multisubst new old (cdr lat)))))))


		   
      