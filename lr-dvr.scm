;; ---------------------------------------------------------------------- ;;
;; FICHIER               : lr-dvr.scm                                     ;;
;; DATE DE CREATION      : Mon May 29 09:51:22 1995                       ;;
;; DERNIERE MODIFICATION : Mon May 29 09:51:39 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; LR(1) parser driver ...                                                ;;
;; ---------------------------------------------------------------------- ;;

(define max-stack-size 500)
(define *debug* #f)

(define (push stack sp new-cat goto-table lval)
  (let* ((state     (vector-ref stack sp))
	 (new-state (cdr (assq new-cat (vector-ref goto-table state))))
	 (new-sp    (+ sp 2)))
    (if (>= new-sp max-stack-size)
	(error push "PARSE ERROR : stack overflow" lval)
	(begin
	  (vector-set! stack new-sp new-state)
	  (vector-set! stack (- new-sp 1) lval)
	  new-sp))))

(define (make-parser action-table goto-table reduction-table token-defs)
  (lambda (lexerp errorp)

    (define (action x l)
      (let ((y (assq x l)))
	(if y 
	    (cdr y) 
	    (cdar l))))
  
    (let ((stack (make-vector max-stack-size 0)))
      (let loop ((sp 0) (input (lexerp)))
	(let* ((state (vector-ref stack sp))
	       (i     (car input))
	       (act   (action i (vector-ref action-table state))))

	  (if *debug*
	      (begin
		(display "** PARSER TRACE: i=") 
		(display (cdr (assq i token-defs)))
		(display "  state=") 
		(display state) 
		(display "  sp=")
		(display sp) 
		(newline)))

	  (cond

	   ;; Input succesfully parsed
	   ((eq? act 'accept)
	    (vector-ref stack 1))

	   ;; Syntax error in input
	   ((eq? act '*error*)
	    (errorp (cdr (assq i token-defs))))

	   ;; Shift current token on top of the stack
	   ((>= act 0)
	    (vector-set! stack (+ sp 1) (cdr input))
	    (vector-set! stack (+ sp 2) act)
	    (loop (+ sp 2) (lexerp)))

	   ;; Reduce by rule (- act)
	   (else 
	    (loop ((vector-ref reduction-table (- act)) stack sp goto-table) 
		  input))))))))

