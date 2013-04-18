;; ---------------------------------------------------------------------- ;;
;; FICHIER               : env.scm                                        ;;
;; DATE DE CREATION      : Mon May 29 09:36:42 1995                       ;;
;; DERNIERE MODIFICATION : Fri Jun  2 11:29:04 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; This file contains all the stuff for environment creation, lookup,     ;;
;; etc.                                                                   ;;
;; ---------------------------------------------------------------------- ;;

;; ***********************************************************************
;; INTERFACE :
;;   . *global-environment* : Variable contenant l'environnement global.
;;   . env:make-binding     : Fonction construisant une liaison.
;;   . env:set-global!      : Initialise l'environnement global.
;;   . env:add-global-env!  : Fonction ajoutant une liaison à l'environnement
;;                            global.
;;   . env:extend-env       : Fonction ajoutant des liaisons à un environnement.
;;   . env:set!             : Fonction qui change la valeur d'une variable
;;                            dans un environnement.
;; ***********************************************************************

(define *global-environment* '())

(define (env:make-binding name type value constant?)
  (error:check-type value type)
  (make-binding name value type constant?))

(define (env:add-global-env! binding)
  (let ((name (binding-name binding)))
    (if (vassoc name *global-environment*)
	(dylan:error "Cannot redefine module variable: %=" (predef:make-symbol name))
	(set-cdr! *global-environment* 
		  (cons binding (cdr *global-environment*))))))

(define (env:set-global! binding)
  (set! *global-environment* 
	(list binding)))

(define (env:extend-env env1 env2)
  (append env1 env2))

(define (env:lookup name env)
  (let ((x (vassoc name env)))
    (if x
	(binding-value x)
	(dylan:error "Unbound variable: %=" (predef:make-symbol name)))))

(define (env:set! new-val binding)
  (if (binding-constant? binding)
      (dylan:error "Attempt to modify constant variable: %=" 
		   (predef:make-symbol (binding-name binding)))
      (begin
	(error:check-type new-val (binding-type binding))
	(binding-value-set! binding new-val))))


;; ---------------------------------------------------------------------- ;;
;; Construction d'un bloc d'activation ...                                ;;
;; ---------------------------------------------------------------------- ;;
(define (link rte size)
  (let ((v (make-vector (+ size 1) #f)))
    (vector-set! v 0 rte)
    v))


(define (vassoc e vl)
  (let loop ((l vl))
    (if (null? l)
	#f
	(let ((v (car l)))
	  (if (eq? (binding-name v) e)
	      v
	      (loop (cdr l)))))))

