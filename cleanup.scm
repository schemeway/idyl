;; ---------------------------------------------------------------------- ;;
;; FICHIER               : cleanup.scm                                    ;;
;; DATE DE CREATION      : Fri May 26 16:22:22 1995                       ;;
;; DERNIERE MODIFICATION : Fri May 26 16:23:09 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; All the stuff for cleanups and blocks...                               ;;
;; ---------------------------------------------------------------------- ;;

;; --- Pile des cleanups ------------------------------------------------ ;;
(define *cleanup-stack* '())

;; --- Code d'une sortie de bloc invalide ------------------------------- ;;
(define exit-proc-error
  (lambda l 
    (dylan:error 
     "Exit proc. called outside of establishing block")))


;; ---------------------------------------------------------------------- ;;
;; Retourne le pointeur de pile de `cleanups'.                            ;;
;; ---------------------------------------------------------------------- ;;
(define (clean:get-top-of-stack)
  *cleanup-stack*)

;; ---------------------------------------------------------------------- ;;
;; Invalide une procedure de sortie...                                    ;;
;; ---------------------------------------------------------------------- ;;
(define (clean:invalidate-exit-proc method)
  (method-code-set! (instance-slots method) exit-proc-error))

;; ---------------------------------------------------------------------- ;;
;; Fonction executant le cleanup le plus recent (suivi d'un depilage).    ;;
;; ---------------------------------------------------------------------- ;;
(define (clean:run-and-pop old-top)
  (let loop ()
    (if (eq? *cleanup-stack* old-top)
	#f
	(let* ((top     (car *cleanup-stack*))
	       (cleanup (car top))
	       (method  (cadr top))
	       (execed? (caddr top)))
	  (if execed?
	      (begin
		(if method
		    (clean:invalidate-exit-proc method))
		(set! *cleanup-stack* (cdr *cleanup-stack*)))
	      (begin
		(set-car! (cddr top) #t)
		(cleanup)
		(set! *cleanup-stack* (cdr *cleanup-stack*))))
	  (loop)))))

;; ---------------------------------------------------------------------- ;;
;; Cette fonction cree un cleanup et le pousse sur la pile des            ;;
;; cleanups. L'argument est un `thunk' qui, lors de son appel,            ;;
;; execute le corps du cleanup.                                           ;;
;; ---------------------------------------------------------------------- ;;
(define (clean:make-cleanup method thunk)
  (set! *cleanup-stack*
	(cons (list thunk method #f) *cleanup-stack*)))


;; ---------------------------------------------------------------------- ;;
;; Fait le ménage dans la pile des cleanups...                            ;;
;; ---------------------------------------------------------------------- ;;
(define (clean:reset-stack)
  (let loop ()
    (if (pair? *cleanup-stack*)
	(let ((method (cadar *cleanup-stack*)))
	  (if method
	      (method-code-set! (instance-slots method) exit-proc-error))
	  (set! *cleanup-stack* (cdr *cleanup-stack*))
	  (loop)))))