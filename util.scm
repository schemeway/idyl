;; ---------------------------------------------------------------------- ;;
;; FICHIER               : util.scm                                       ;;
;; DATE DE CREATION      : Mon May 29 09:58:52 1995                       ;;
;; DERNIERE MODIFICATION : Mon May 29 10:01:34 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Miscellaneous functions and variables ...                              ;;
;; ---------------------------------------------------------------------- ;;

;; ---------------------------------------------------------------------- ;;
;; A few constants ...                                                    ;;
;; ---------------------------------------------------------------------- ;;

(define $all-keys-node    (make-all-keys))

(define $the-empty-list   (make-instance #f #f))
(define $the-true-value   (make-instance #f #t))
(define $the-false-value  (make-instance #f #f))

(define $non-initialized '(uninitialized))
(define $unbound         '(unbound))


(define vertical-bar      (string->symbol "|"))

;; ---------------------------------------------------------------------- ;;
;; Used in parser.scm ...                                                 ;;
;; ---------------------------------------------------------------------- ;;
(define (insert-in-condition-clause! x c)
  (cond-clause-matches-set! c (cons x (cond-clause-matches c))))

;; ---------------------------------------------------------------------- ;;
;; Sorted insertion function                                              ;;
;; ---------------------------------------------------------------------- ;;

(define (sinsert elem lst)
  (let loop ((l1 lst))
    (if (null? l1) 
	(cons elem l1)
	(let ((x (car l1)))
	  (cond ((< elem x)
		 (cons elem l1))
		((> elem x)
		 (cons x (loop (cdr l1))))
		(else 
		 l1))))))

;; ---------------------------------------------------------------------- ;;
;; The operator precedence list ...                                       ;;
;; ---------------------------------------------------------------------- ;;
(define $precedence-list
  (list
   (cons '^ 2)
   (cons '* 3) (cons '/ 3)
   (cons '+ 4) (cons '- 4)
   (cons '= 5) (cons '== 5) (cons '~= 5) (cons '< 5) (cons '> 5)
               (cons '<= 5) (cons '>= 5)
   (cons '& 6) (cons vertical-bar 6)
   (cons ':= 7)))

(define (op->precedence op)
  (cdr (assq op $precedence-list)))

(define (binop-series->expr lst)
  (define (make-node v1 op v2)
    (let ((name (binary-op-name op)))
      (cond
       ((eq? name ':=)
	(make-assignment v1 v2))
       ((eq? name '&)
	(make-and-expr v1 v2))
       ((eq? name vertical-bar) 
	(make-or-expr v1 v2))
       (else
	(make-funcall (make-ast-symbol name) (list v1 v2))))))
  
  (define (make-exprn val-stack op-stack pr-stack lst)
    (if (null? op-stack)
	(if (null? lst)
	    (car val-stack)
	    (let* ((op (car lst))
		   (pr (op->precedence (binary-op-name op)))
		   (val (cadr lst)))
	      (make-exprn (cons val val-stack) 
			 (cons op op-stack) 
			 (cons pr pr-stack)
			 (cddr lst))))
	(if (null? lst)
	    (let ((node (make-node (cadr val-stack) (car op-stack) (car val-stack))))
	      (make-exprn (cons node (cddr val-stack)) 
			 (cdr op-stack) 
			 (cdr op-stack) 
			 lst))
	    (let* ((r-op (car lst))
		   (l-op (car op-stack))
		   (r-pr (op->precedence (binary-op-name r-op)))
		   (l-pr (car pr-stack))
		   (r-val (cadr lst))
		   (rest (cddr lst)))
	      (cond
	       ((< r-pr l-pr)
		(make-exprn (cons r-val val-stack) 
			   (cons r-op op-stack)
			   (cons r-pr pr-stack)
			   rest))
	       ((> r-pr l-pr)
		(let* ((l-val1 (cadr val-stack))
		       (l-val2 (car val-stack))
		       (node (make-node l-val1 l-op l-val2))
		       (val-rest (cddr val-stack)))
		  (make-exprn (cons node val-rest)
			     (cdr op-stack)
			     (cdr pr-stack)
			     lst)))
	       (else			; r-pr = l-pr
		(if (eq? (binary-op-name r-op) ':=)
		    (make-exprn (cons r-val val-stack)
			       (cons r-op  op-stack)
			       (cons r-pr  pr-stack)
			       rest)
		    (let* ((l-val1 (cadr val-stack))
			   (l-val2 (car val-stack))
			   (node (make-node l-val1 l-op l-val2))
			   (val-rest (cddr val-stack)))
		      (make-exprn (cons node val-rest)
				 (cdr op-stack)
				 (cdr pr-stack)
				 lst)))))))))
  
  (make-exprn (list (car lst)) '() '() (cdr lst)))
		    
;; ---------------------------------------------------------------------- ;;
;; The filter function ...                                                ;;
;; ---------------------------------------------------------------------- ;;
(define (filter pred l)
  (if (null? l)
      '()
      (let ((elt (car l)))
	(if (pred elt)
	    (cons elt (filter pred (cdr l)))
	    (filter pred (cdr l))))))