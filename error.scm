;; ---------------------------------------------------------------------- ;;
;; FICHIER               : error.scm                                      ;;
;; DATE DE CREATION      : Mon May 29 09:39:15 1995                       ;;
;; DERNIERE MODIFICATION : Fri Jun  2 11:14:14 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; The stuff for error and condition processing                           ;;
;; ---------------------------------------------------------------------- ;;

;; ---------------------------------------------------------------------- ;;
;; get-first-val : extrait la première valeur d'une liste de résultats.   ;;
;; ---------------------------------------------------------------------- ;;
(define-macro (get-first-val vals)
  `(let ((v ,vals))
     (if (list? v) (if (null? v) $the-false-value (car v)) v)))


;; ---------------------------------------------------------------------- ;;
;; Initialisation du module d'erreurs ...                                 ;;
;; ---------------------------------------------------------------------- ;;
(define (error:init)
  (set! $value-key  (make-instance <symbol> 'value))
  (set! $type-key   (make-instance <symbol> 'type))
  (set! $format-str (make-instance <symbol> 'format-string))
  (set! $format-arg (make-instance <symbol> 'format-arguments)))

;; ---------------------------------------------------------------------- ;;
;; Quelques continuations à conserver ...                                 ;;
;; ---------------------------------------------------------------------- ;;
(define **error-cont** #f)
(define **quit-cont**  #f)

(define $value-key  #f)
(define $type-key   #f)
(define $format-str #f)
(define $format-arg #f)

(define-macro (make-handler type test init handl)
  `(vector ,type ,test ,init ,handl))
(define-macro (handler-type  handl) `(vector-ref ,handl 0))
(define-macro (handler-test  handl) `(vector-ref ,handl 1))
(define-macro (handler-init  handl) `(vector-ref ,handl 2))
(define-macro (handler-handl handl) `(vector-ref ,handl 3))

;; ---------------------------------------------------------------------- ;;
;; L'environnement dynamique des exceptions...                            ;;
;; ---------------------------------------------------------------------- ;;
(define *condition-stack* '())

;; ---------------------------------------------------------------------- ;;
;; Retourne le pointeur de dessus de pile des exceptions                  ;;
;; ---------------------------------------------------------------------- ;;
(define (error:get-top-of-stack) *condition-stack*)

;; ---------------------------------------------------------------------- ;;
;; Etablissement d'un `handler' pour une exception. Retourne le pointeur  ;;
;; de pile avant l'établissement.                                         ;;
;; ---------------------------------------------------------------------- ;;
(define (error:establish-handler type test init-args handler)
  (error:check-type handler <function>)
  (if (class:subtype? type <condition>)
      (set! *condition-stack* (cons (make-handler type test init-args handler)
				    *condition-stack*))
      (dylan:error "[* TYPE ERROR: handler type is not a <condition> *]")))

;; ---------------------------------------------------------------------- ;;
;; Rétablissement d'un ancien pointeur de pile d'exception (en sortant    ;;
;; d'un bloc, par exemple).                                               ;;
;; ---------------------------------------------------------------------- ;;
(define (error:pop-handlers oldp) (set! *condition-stack* oldp))

;; ---------------------------------------------------------------------- ;;
;; Signalement d'une condition ...                                        ;;
;; ---------------------------------------------------------------------- ;;
(define (error:signal cnd)
  (let loop ((l *condition-stack*))
    (if (null? l)
	(fun:apply default-handler-gf (list cnd) #f)
	(let* ((handl (car l))
	       (rest  (cdr l))
	       (htype (handler-type handl))
	       (htest (handler-test handl)))
	  (if (and (class:general-instance? cnd htype)
		   (not (eq? $the-false-value
			     (get-first-val (fun:apply htest (list cnd) #f)))))
	      (fun:apply
	       (handler-handl handl)
	       (list cnd
		     (fun:make-predef
		      'anonymous '() #f '() #f 
		      (make-return-value '() #f #f)
		      (lambda (args) (loop rest))))
	       #f)
	      (loop rest))))))
		      
;; ---------------------------------------------------------------------- ;;
;; Vérification de type                                                   ;;
;; ---------------------------------------------------------------------- ;;
(define (error:check-type value type)
  (if (class:general-instance? value type)
      value
      (error:signal 
       (class:make <type-error> $value-key value $type-key type))))


;; ---------------------------------------------------------------------- ;;
;; ABORT ...                                                              ;;
;; ---------------------------------------------------------------------- ;;
(define (error:abort)
  (**error-cont** #f))

;; ---------------------------------------------------------------------- ;;
;; Capture la continuation pour une erreur fatale ...                     ;;
;; ---------------------------------------------------------------------- ;;
(define (catch-error thunk)
  (bind-exit (cont)
    (set! **error-cont** cont)
    (catch-signals thunk)))

;; ---------------------------------------------------------------------- ;;
;; Capture la continuation pour la sortie ...                             ;;
;; ---------------------------------------------------------------------- ;;
(define (catch-quit-cont thunk)
  (set! **quit-cont** thunk))

(define (call-quit-cont)
  (**quit-cont** #t))

;; ---------------------------------------------------------------------- ;;
;; Fonctions d'erreur ... À modifier                                      ;;
;; ---------------------------------------------------------------------- ;;
(define (dylan:error str . objs)
  (error:signal
   (class:make <simple-error> 
	       $format-str
	       (make-instance <byte-string> str)
	       $format-arg
	       (predef:make-list objs)))
  (error:abort))

(define (dylan:warning str . objs)
  (error:signal
   (class:make <simple-warning>
	       $format-str
	       (make-instance <byte-string> str)
	       $format-arg
	       (predef:make-list objs))))

;; ---------------------------------------------------------------------- ;;
;; Impression d'un objet                                                  ;;
;; ---------------------------------------------------------------------- ;;
(define (display-obj obj)
  (if (instance? obj)
      (let ((class (instance-class obj)))
	(cond
	 ((eq? obj $the-true-value)
	  (display "#t"))
	 ((eq? obj $the-false-value)
	  (display "#f"))
	 ((eq? obj $the-empty-list)
	  (display "#()"))
	 ((eq? class <class>)
	  (display "{class ")
	  (display (class-name (instance-slots obj)))
	  (display "}"))
	 ((eq? class <singleton>)
	  (display "{sigleton ")
	  (display-obj (instance-slots obj))
	  (display "}"))
	 ((eq? class <user-method>)
	  (display "{method \"")
	  (display (method-name (instance-slots obj)))
	  (display "\"}"))
	 ((eq? class <compiled-method>)
	  (display "{compiled-method \"")
	  (display (method-name (instance-slots obj)))
	  (display "\"}"))
	 ((eq? class <byte-string>)
	  (display "\"") (display (instance-slots obj)) (display "\""))
	 ((eq? class <integer>)
	  (display (instance-slots obj)))
	 ((eq? class <double-float>)
	  (display (instance-slots obj)))
	 ((eq? class <character>)
	  (display "'") (display (instance-slots obj)) (display "'"))
	 ((eq? class <symbol>)
	  (display "#\"")
	  (display (instance-slots obj))
	  (display "\""))
	 (else
	  (display "{instance of ")
	  (display (class-name (instance-slots class)))
	  (display "}"))))
      (display obj)))

;; ---------------------------------------------------------------------- ;;
;; Impression d'un objet et retour de chariot ...                         ;;
;; ---------------------------------------------------------------------- ;;
(define (pretty-obj obj)
  (display-obj obj)
  (newline))


;; ---------------------------------------------------------------------- ;;
;; Impression d'une "format string" ...                                   ;;
;; ---------------------------------------------------------------------- ;;
(define (display-format-string str . objs)
  (let loop ((chars (string->list str)) (objs objs))
    (cond
     ((pair? chars)
      (let ((c    (car chars))
	    (rest (cdr chars)))
	(if (char=? c #\%)
	    (if (null? rest)
		(display #\%)
		(let ((c2  (char-downcase (car rest)))
		      (obj (if (pair? objs) (car objs) #f)))
		  (cond
		   ((not obj)
		    (display c2))
		   ((or (char=? c2 #\d)
			(char=? c2 #\b)
			(char=? c2 #\o)
			(char=? c2 #\x))
		    (if (class:general-instance? obj <integer>)
			(display (instance-slots obj))
			(display "<not an integer>"))
		    (loop (cdr rest) (cdr objs)))
		   ((char=? c2 #\c)
		    (if (class:general-instance? obj <character>)
			(display (instance-slots obj))
			(display "<not a character>"))
		    (loop (cdr rest) (cdr objs)))
		   ((char=? c2 #\s)
		    (cond
		     ((class:general-instance? obj <byte-string>)
		      (display (instance-slots obj)))
		     ((class:general-instance? obj <condition>)
		      (let ((str (fun:apply (binding-value error-message)
					    (list obj)
					    #f)))
			(display (instance-slots str))))
		     (else
		      (display "<not a string or condition>")))
		    (loop (cdr rest) (cdr objs)))
		   ((char=? c2 #\=)
		    (display-obj obj)
		    (loop (cdr rest) (cdr objs)))
		   (else
		    (display c2)
		    (loop (cdr rest) objs)))))
	    (begin
	      (display c)
	      (loop rest objs)))))
     (else #f))))
