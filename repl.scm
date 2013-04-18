;; ---------------------------------------------------------------------- ;;
;; FICHIER               : repl.scm                                       ;;
;; DATE DE CREATION      : Mon May 29 09:54:57 1995                       ;;
;; DERNIERE MODIFICATION : Thu Jun  8 15:59:14 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; IDyl Read-Eval-Print loop...                                           ;;
;; ---------------------------------------------------------------------- ;;

(define *emacs-mode?* #f)

;; --- Version de l'interprète ... -------------------------------------- ;;
(define-macro (version-string) "/** IDyl v0.1    (05/25/95)     **/")


;; ---------------------------------------------------------------------- ;;
;; La boucle REP                                                          ;;
;; ---------------------------------------------------------------------- ;;
(define (dylan:repl argv)
;; --- initialisations globales ----------------------------------------- ;;
  (global:init)

  (bind-exit (cont)
     (catch-quit-cont cont)

;; --- charge les fichiers usagers ... ---------------------------------- ;;
     (catch-error
      (lambda ()
	(for-each
	 (lambda (arg)
	   (cond
	    ((string=? arg "-emacs")
	     (set! *emacs-mode?* #t))
	    (else
	     (fun:apply
	      predef:load
	      (list (make-instance <byte-string> arg))
	      #f))))
	 (cdr argv))))

;; --- imprime le message ----------------------------------------------- ;;
     (for-each
      (lambda (str) (display str) (newline))
      (list
       "/*********************************/"
       (version-string)
       "/** Entering Dylan REP loop     **/"
       "/** Type `quit ()' to exit      **/"
       "/*********************************/"))
     
;; --- la boucle REP ---------------------------------------------------- ;;
     (let loop ()
       (catch-error
	(lambda ()
	  (clean:reset-stack)
	  (error:pop-handlers '())
	  (class:reset-sealing-contexts)
	  (let boucle ((ast (dylan:load (current-input-port) (not *emacs-mode?*))))
	    (if (null? ast) 
		(boucle (dylan:load (current-input-port) (not *emacs-mode?*)))
		(let* ((code (gen:gen-constituent (car ast)))
		       (vals (code #f)))
		  (for-each 
		   (lambda (v)
		     (display "// value = ")
		     (pretty-obj v))
		   (if (lst? vals) vals (list vals)))
		  (boucle (cdr ast)))))))
       (loop))))

;; ---------------------------------------------------------------------- ;;
;; Appel de la routine principale ...                                     ;;
;; ---------------------------------------------------------------------- ;;
(main)
