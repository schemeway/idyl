;; ---------------------------------------------------------------------- ;;
;; FICHIER               : class.scm                                      ;;
;; AUTEUR                : Dominique Boucher                              ;;
;; DATE DE CREATION      : Mon Feb 27 14:23:08 EST 1995                   ;;
;; DERNIERE MODIFICATION : Thu Jun  8 16:26:54 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Ce fichier contient le protocole de création et de manipulation        ;;
;; des classes, des instances et des singletons Dylan.                    ;;
;; ---------------------------------------------------------------------- ;;

;; ***********************************************************************
;; class:add-subclass : ajoute c2 à la liste des sous-classes de c1.
;; ***********************************************************************
(define-macro (class:add-subclass! c1 c2)
  `(CLASS-DIRECT-SUBS-SET! ,c1 (CONS ,c2 (CLASS-DIRECT-SUBS ,c1))))

(define (make-sym . l) (dylan:string->symbol (apply string-append l)))

;; ---------------------------------------------------------------------- ;;
;; List of classes to seal ...                                            ;;
;; ---------------------------------------------------------------------- ;;
(define *classes-to-seal* '(()))

;; ---------------------------------------------------------------------- ;;
;; Push a new context for class sealing  ...                              ;;
;; ---------------------------------------------------------------------- ;;
(define (class:push-new-sealing-context)
  (set! *classes-to-seal* (cons '() *classes-to-seal*)))

;; ---------------------------------------------------------------------- ;;
;; Add a new class for sealing in the current sealing context ...         ;;
;; ---------------------------------------------------------------------- ;;
(define (class:add-class-to-seal cl)
  (set-car! *classes-to-seal* (cons cl (car *classes-to-seal*))))

;; ---------------------------------------------------------------------- ;;
;; Function that seals all the classes waiting for it...                  ;;
;; ---------------------------------------------------------------------- ;;
(define (class:seal-all-and-pop)
  (if (null? *classes-to-seal*)
      (dylan:error "** internal error in seal-all-and-pop"))
  (let loop ((l (car *classes-to-seal*)))
    (if (pair? l)
	(begin
	  (class-sealed?-set! (car l) #t)
	  (loop (cdr l)))
	(set! *classes-to-seal* (cdr *classes-to-seal*)))))

(define (class:reset-sealing-contexts)
  (let loop ((len (length *classes-to-seal*)))
    (if (> len 1)
	(begin
	  (class:seal-all-and-pop)
	  (loop (- len 1))))))


;; ***********************************************************************
;; class:make-class : construit une classe à partir de sa description.
;; ***********************************************************************
(define (class:make-class name flags supers slot-descr)
  (let* ((cl (make-class name #f #f #f #f '() '() '() '() '() '()))
	 (inst (make-instance <class> cl)))
    (class:parse-flags! cl flags)
    (class:set-supers! cl inst supers)
    (class-prec-list-set!
     cl
     (class:make-prec-list 
      inst
      (let loop ((l (class-supers cl)))
	(if (null? l)
	    '()
	    (cons (class-prec-list (instance-slots (car l))) (loop (cdr l)))))))
    (class:check-primary-classes cl)
    (class:parse-slots! cl inst slot-descr)
    inst))


;; ***********************************************************************
;; class:parse-flags : extrait les qualificatifs de la classe
;; ***********************************************************************
(define (class:parse-flags! cl flags)
  (let loop ((l flags) (open-flag? #f) (sealed-flag? #f))
    (if (null? l)
	(if (not open-flag?)
	    (class:add-class-to-seal cl))
	(let ((x (car l)))
	  (cond 
	   ((eq? x 0) 
	    (loop (cdr l) open-flag? #t))
	   ((eq? x 1)
	    (if sealed-flag?
		(dylan:error "Class cannot be both `sealed' and `open': %=" 
			     (make-instance <symbol> (class-name cl)))
		(loop (cdr l) #t sealed-flag?)))
	   ((eq? x 2)
	    (class-abstract?-set! cl #t)
	    (loop (cdr l) open-flag? sealed-flag?))
	   ((eq? x 3) 
	    (if (class-abstract? cl)
		(dylan:error "Class cannot be both `abstract' and `concrete': %=" 
			     (make-instance <symbol> (class-name cl)))
		(loop (cdr l) open-flag? sealed-flag?)))
	   ((eq? x 4)
	    (class-primary?-set! cl #t)
	    (loop (cdr l) open-flag? sealed-flag?))
	   ((eq? x 5) 
	    (if (class-primary? cl)
		(dylan:error "Class cannot be both `primary' and `free': %=" 
			     (make-instance <symbol> (class-name cl)))
		(loop (cdr l) open-flag? sealed-flag?))))))))


;; ***********************************************************************
;; class:set-supers! : vérifie que l'hétérarchie est consistante
;; ***********************************************************************
(define (class:set-supers! cl inst supers)
  ; détecte une redondance dans la liste des superclasses.
  (let loop ((l supers))
    (if (pair? l)
	(let* ((super (car l))
	       (the-class (instance-slots super))
	       (rest (cdr l)))
	  (error:check-type super <class>)
	  (if (memq super rest)
	      (dylan:error "Superclass cannot appear twice"))
	  (if (class-sealed? the-class)
	      (dylan:error "Trying to subclass a sealed class!"))
	  (loop rest))))

  ; Initialise les super-classes de cl.
  (class-supers-set! cl supers)

  ; Ajoute l'instance comme sous-classe de ces super-classes.
  (let loop ((l supers))
    (if (pair? l)
	(let ((c (instance-slots (car l))))
	  (class:add-subclass! c inst)
	  (loop (cdr l))))))

;; ***********************************************************************
;; class:make-prec-list : cette fonction bâtit la liste de précédence 
;; d'une classe à partir de celles de ses superclasses.
;; ***********************************************************************
(define (class:make-prec-list inst super-lists)
  (letrec 
      ((strip (lambda (a) 
		(lambda (lst)
		   ; test la consistence de l'hétérarchie
		  (if (null? lst)
		      (dylan:error "Inconsistent CPL"))
		  (if (eq? a (car lst))
		      (cdr lst)
		      ; test la consistence de l'hétérarchie
		      (if (memq a lst)
			  (dylan:error "Inconsistent CPL")
			  lst)))))
       (loop (lambda (lsts cpl)
	       (cond 
		((null? lsts) 
		 (cons inst cpl))
		((null? (car lsts))
		 (loop (cdr lsts) cpl))
		(else
		 (let ((element (caar lsts))
		       (rest (cdar lsts)))
		   (loop (map (strip element) lsts) (cons element cpl))))))))
    (loop (reverse (map reverse super-lists)) '())))


;; ---------------------------------------------------------------------- ;;
;; Check fo primary superclasses consistency ...                          ;;
;; ---------------------------------------------------------------------- ;;
(define (class:check-primary-classes cl)
  (let loop ((l      (class-supers cl))
	     (prim-l '()))
    (if (null? l)
	#t
	(let ((super (car l)))
	  (if (class-primary? (instance-slots super))
	      (let loop2 ((l2 prim-l))
		(if (null? l2)
		    (loop (cdr l) (cons super prim-l))
		    (let ((x (car l2)))
		      (if (or (class:subtype? x super)
			      (class:subtype? super x))
			  (loop2 (cdr l2))
			  (dylan:error "Too many primary superclasses")))))
	      (loop (cdr l) prim-l))))))

;; ***********************************************************************
;; class:parse-slots! : initialise les descripteurs de `slot' de cl.
;; ***********************************************************************
(define (class:parse-slots! cl inst slot-descr)

  (define *g/s-set* '())
  (define (add-g/s-name! name class)
    (set! *g/s-set* (cons (cons name class) *g/s-set*)))
  ;; Vérifie qu'il n'y a pas de conflit de noms
  ;; ==> retourne #t si la slot est déjà définie par la même classe.
  (define (name-conflict? name class)
    (let ((x (assoc name *g/s-set*)))
      (if x
	  (if (eq? class (cdr x))
	      #t
	      (dylan:error "Conflict between getter/setter names: %=" 
			   (make-instance <symbol> name)))
	  #f)))
  
  (define *slots* '())
  (define (add-slot! getter sl)
    (set! *slots* (cons (cons getter sl) *slots*)))
  
  (define *init-args* '())
  (define *init-args-conflicts* '())
  
  (define (compatible-init-args arg1 arg2) ; arg2 est hérité ...
    (and (class:subtype? (init-arg-type arg1) (init-arg-type arg2))))

  (define (required-arg? arg1 arg2)
    (or (init-arg-required? arg1)
	    (and (init-arg-required? arg2)
		 (not (init-arg-init-value arg1))
		 (not (init-arg-init-function arg1)))))
  
  (define (resolve-conflict! arg-name arg arg-l)
    (let ((compat? (let loop ((l arg-l))
		     (if (null? l) 
			 #t 
			 (if (compatible-init-args arg (car l)) (loop (cdr l)) #f)))))
      (if (not compat?)
	  (dylan:error "Non-compatible initialization argument: %=" 
		       (make-instance <symbol> arg-name))))
    (if (= (length arg-l) 1)
	(let ((arg2 (car arg-l)))
	  (init-arg-required?-set! arg (required-arg? arg arg2))
	  (if (and (not (init-arg-init-value arg))
		   (not (init-arg-init-function arg)))
	      (cond
		((init-arg-init-value arg2)
		 (init-arg-init-value-set! arg (init-arg-init-value arg2)))
		((init-arg-init-function arg2)
		 (init-arg-init-function-set! arg (init-arg-init-function arg2)))))))
    (set! *init-args* (cons (cons arg-name arg) *init-args*)))
  
  (define (add-inherited-arg! name arg)
    (let ((x (assq name *init-args-conflicts*)))
      (if x 
	  (let ((args (cdr x)))
	    (if (not (memq arg args))
		(set-cdr! x (cons arg args))))
	  (set! *init-args-conflicts* 
		(cons (list name arg) *init-args-conflicts*)))))
  
  (define (add-init-arg! name arg)
    (if (assq name *init-args*)
	(dylan:error "More than one init. arg. specification for: %="
		     (make-instance <symbol> name)))
    (let ((x (assq name *init-args-conflicts*)))
      (if x
	  (resolve-conflict! name arg (cdr x))
	  (set! *init-args* (cons (cons name arg) *init-args*)))))
  
  (define (parse-init-arg-gut req? prop-list)
    (let ((init-arg (make-init-arg req? #f #f #f)))
      (let loop ((l prop-list))
	(if (pair? l)
	    (let ((key (caar l))
		  (val (cdar l)))
	      (cond
	       ((eq? key 'TYPE)
		(cond
		 ((init-arg-type init-arg)
		  (dylan:warning "`type:' is specified twice in init. arg."))
		 (else
		  (let ((type ((gen:gen-expr '() val) #f)))
		    (error:check-type type <type>)
		    (init-arg-type-set! init-arg type)))))
	       
	       ((eq? key 'INIT-VALUE)
		(cond
		 ((init-arg-init-value init-arg)
		  (dylan:warning "`init-value:' is specified twice in init. arg."))
		 ((init-arg-init-function init-arg)
		  (dylan:error "`init-function:' and `init-value:' in init. arg."))
		 (req? 
		  (dylan:error "`init-value:' cannot be specified for required init. arg."))
		 (else
		  (let ((initv ((gen:gen-expr '() val) #f)))
		    (init-arg-init-value-set! init-arg initv)))))

	       ((eq? key 'INIT-FUNCTION)
		(cond
		 ((init-arg-init-function init-arg)
		  (dylan:warning "`init-function:' is specified twice in init. arg."))
		 ((init-arg-init-value init-arg)
		  (dylan:error "`init-function' and `init-value:' in init. arg."))
		 (req? 
		  (dylan:error 
		   "`init-function:' cannot be specified for required init. arg."))
		 (else
		  (let ((initf ((gen:gen-expr '() val) #f)))
		    (init-arg-init-function-set! init-arg initf)))))
	       (else
		(dylan:error "Unrecognized init. arg. keyword: `%=:'" 
			     (make-instance <symbol> key))))
	      
	      (loop (cdr l)))))
      (if (not (init-arg-type init-arg))
	  (init-arg-type-set! init-arg <object>))
      init-arg))
  
  (define (finalize-init-args!)
    (let loop ((l *init-args-conflicts*))
      (if (pair? l)
	  (let ((name (caar l))
		(descrs (cdar l)))
	    (if (assq name *init-args*)
		(loop (cdr l))
		(if (= (length descrs) 1)
		    (begin
		      (set! *init-args* (cons (cons name (car descrs)) *init-args*))
		      (loop (cdr l)))
		    (dylan:error "Unresolved conflicts between init. args.: %=" 
				 (make-instance <symbol> name))))))))

  
  (let loop ((supers (class-supers cl)))
    (if (pair? supers)
	(let ((super (car supers)))
	  ; On commence par trouver les `slots' des superclasses.
	  (let slots-loop ((l (class-slots (instance-slots super))))
	    (if (pair? l)
		(let* ((slot (car l))
		       (slot-descr (cdr slot))
		       (slot-getter (slot-getter-name slot-descr))
		       (slot-setter (slot-setter-name slot-descr))
		       (def-class   (slot-class-def slot-descr))
		       (rest (cdr l)))
		  
		  ; on vérifie qu'il n'y a pas de redondance dans les noms
		  (cond
		   ((name-conflict? slot-getter def-class)
		    (slots-loop rest))
		   ((name-conflict? slot-setter def-class)
		    (slots-loop rest))
		   (else
		    (add-g/s-name! slot-getter def-class)
		    (add-g/s-name! slot-setter def-class)
		    ; Doit-on ajouter le champ à l'instance de class ?
		    (add-slot! slot-getter slot-descr)
		    (slots-loop rest))))))

	  (let inits-loop ((l (class-init-args (instance-slots super))))
	    (if (pair? l)
		(let* ((arg (car l))
		       (arg-name (car arg))
		       (arg-descr (cdr arg)))
		  (add-inherited-arg! arg-name arg-descr)
		  (loop (cdr l)))))
	  
	  (loop (cdr supers)))))
  
  ; On ajoute les `slots' qui ne sont pas hérités.
  (let loop ((sl slot-descr))
    (if (null? sl)
	(class-slots-set! cl *slots*)
	(let* ((descr (car sl)) (descr-type (car descr)))
	  (cond 
	   
	   ; Définition d'un nouveau champ
	   ((eq? descr-type 'SLOT)
	    (let ((sl-name (list-ref descr 1))
		  (sl-type (list-ref descr 4)))

	      (let* ((slot-str    (symbol->string sl-name))
		     (setter-name (make-sym slot-str "-setter"))
		     (alloc       (list-ref descr 3))
		     (slot        (make-slot 
				   inst sl-name #f
				   sl-type #f #f #f #f #f alloc)))
		
		(class:parse-slot-keys! slot (list-ref descr 5))
		
		(if (not (eq? alloc 'CONSTANT))
		    (if (not (slot-setter-name slot))
			(slot-setter-name-set! slot setter-name))
		    (slot-setter-name-set! slot $the-false-value))
		
		(if (name-conflict? (slot-getter-name slot) cl)
		    (dylan:error "Conflict between getter/setter names: %=" 
				 (make-instance <symbol> (slot-getter-name slot))))
		(if (name-conflict? (slot-setter-name slot) cl)
		    (dylan:error "Conflict between getter/setter names: %=" 
				 (make-instance <symbol> (slot-setter-name slot))))
		
		(add-g/s-name! (slot-getter-name slot) cl)
		(if (and (not (eq? alloc 'CONSTANT))
			 (not (eq? (slot-setter-name slot) $the-false-value)))
		    (add-g/s-name! (slot-setter-name slot) cl))
		(add-slot! (slot-getter-name slot) slot))))
	   
	   ; Définition d'un mot-clé
	   ((eq? descr-type 'KEYWORD)
	    (add-init-arg! 
	     (cadr descr) 
	     (parse-init-arg-gut (caddr descr) (cadddr descr))))

	   ; Modifications à un champ hérité
	   ((eq? descr-type 'INHERITED-SLOT)
	    (let* ((slot-name (list-ref descr 1))
		   (slot-keys (list-ref descr 2))
		   (slot (assq slot-name *slots*)))
	      (if slot
		  (set-cdr! slot (class:parse-inherited-keys! (cdr slot) slot-keys))
		  (dylan:error "Undefined inherited slot: %=" 
			       (make-instance <symbol> slot-name))))))
	   
	  (loop (cdr sl)))))
  (finalize-init-args!)
  (class-init-args-set! cl *init-args*))


;; ***********************************************************************
;; class:parse-slot-keys! : parcourt les mots-clés d'une définition de
;;   slot.
;; ***********************************************************************
(define (class:parse-slot-keys! slot prop-list)
  (let loop ((l prop-list))
    (if (pair? l)
	(let ((key (caar l))
	      (val (cdar l)))
	  (cond

	   ((eq? key 'SETTER)
	    (cond
	     ((slot-setter-name slot)
	      (dylan:warning "`setter:' cannot be specified twice (ignored)"))
	     ((false-constant? val)
	      (slot-setter-name-set! slot $the-false-value))
	     ((ast-symbol? val)
	      (if (eq? (slot-allocation slot) 'CONSTANT)
		  (dylan:warning "Setter specified for constant slot (ignored)")
		  (slot-setter-name-set! slot (ast-symbol-name val))))
	     (else
	      (dylan:error "Bad setter name"))))
	   
	   ((eq? key 'INIT-VALUE)
	    (cond
	     ((slot-init-value slot)
	      (dylan:warning "`init-value:' cannot be specified twice in slot decl."))
	     ((slot-init-function slot)
	      (dylan:error "Slot decl. has `init-value:' and `init-function:'"))
	     ((slot-req-init-key slot)
	      (dylan:error "Slot decl. has `init-value:' and `required-init-keyword:'"))
	     (else
	      (let ((comp-val (gen:gen-expr '() val)))
		(slot-init-value-set! slot comp-val)))))

	   ((eq? key 'INIT-FUNCTION)
	    (cond
	     ((slot-init-function slot)
	      (dylan:warning "`init-function:' cannot be specified twice in slot decl."))
	     ((slot-init-value slot)
	      (dylan:error "Slot decl. has `init-value:' and `init-function:'"))
	     ((slot-req-init-key slot)
	      (dylan:error "Slot decl. has `init-function:' and `required-init-keyword:'"))
	     (else
	      (let ((comp-val (gen:gen-expr '() val)))
		(slot-init-function-set! slot comp-val)))))

	   ((eq? key 'INIT-KEYWORD)
	    (cond
	     ((slot-init-key slot)
	      (dylan:warning "`init-keyword:' cannot be specified twice in slot decl."))
	     ((slot-req-init-key slot)
	      (dylan:error "Slot decl. has `init-keyword:' and `required-init-keyword:'"))
	     ((not (keyword? val))
	      (dylan:error "Illegal keyword!!"))
	     (else
	      (slot-init-key-set! slot (keyword-name val)))))

	   ((eq? key 'REQUIRED-INIT-KEYWORD)
	    (cond
	     ((slot-req-init-key slot)
	      (dylan:warning "`required-init-keyword:' cannot be specified twice in slot decl."))
	     ((slot-init-key slot)
	      (dylan:error "Slot decl. has `init-keyword:' and `required-init-keyword:'"))
	     ((not (keyword? val))
	      (dylan:error "Illegal keyword!!"))
	     (else
	      (slot-req-init-key-set! slot (keyword-name val)))))
		 
	   (else
	    (dylan:error "Keyword not handled in dylan:make-class!: %="
			 (make-instance <symbol> key))))
	  (loop (cdr l))))))

;; ***********************************************************************
;; class:parse-inherited-keys! : parcourt les mots-clés d'une définition de
;;   slot hérité.
;; ***********************************************************************
(define (class:parse-inherited-keys! the-slot keys)
  (define *new-slot*
    (make-slot (slot-class-def the-slot) (slot-getter-name the-slot)
	       (slot-setter-name the-slot) (slot-type the-slot)
	       (slot-deferred-type the-slot) (slot-init-value the-slot)
	       (slot-init-function the-slot) (slot-init-key the-slot)
	       (slot-req-init-key the-slot) (slot-allocation the-slot)))
  (if (> (length keys) 1)
      (dylan:error "Too many options in inherited slot spec."))
  (if (pair? keys)
      (let ((key (caar keys))
	    (val (cdar keys)))
	(cond
	 ((eq? key 'INIT-VALUE)
	  (slot-init-value-set! *new-slot* (gen:gen-expr '() val))
	  (slot-init-function-set! *new-slot* #f))
	 ((eq? key 'INIT-FUNCTION)
	  (slot-init-function-set! *new-slot* (gen:gen-expr '() val))
	  (slot-init-value-set! *new-slot* #f))
	 (else
	  (dylan:error "Invalid keyword in inherited slot spec.")))))
  *new-slot*)

;; ***********************************************************************
;; class:adjust-slots! : évalue `type:', `init-value:' et `init-function:'
;;   après la création d'une classe.
;; ***********************************************************************
(define (class:adjust-slots! cl rte)
  (let loop ((slots (class-slots (instance-slots cl))))
    (if (pair? slots)
	(let* ((slot (car slots))
	       (slot-descr (cdr slot)))
	  (let ((type (slot-type slot-descr)))
	    (if (procedure? type)
		(let ((evaled-type (type rte)))
		  (error:check-type evaled-type <type>)
		  (slot-type-set! slot-descr evaled-type))))
	  (let ((init-val (slot-init-value slot-descr)))
	    (if (and init-val (procedure? init-val))
		(slot-init-value-set! slot-descr (init-val rte))))
	  (let ((init-fun (slot-init-function slot-descr)))
	    (if (and init-fun (procedure? init-fun))
		(slot-init-function-set! slot-descr (init-fun rte))))
	  (loop (cdr slots))))))
    
    
;; Déclaration des opérations de base sur les classes
;; =================================================

;; ***********************************************************************
;; class:subtype? : détermine si t1 est un sous-type de t2
;; ***********************************************************************
(define (class:subtype? t1 t2)
  (if (memq t2 (class-prec-list (instance-slots t1))) #t #f))

;; ***********************************************************************
;; class:general-instance? : détermine si obj est une instance générale de class
;; ***********************************************************************
(define (class:general-instance? obj class)
  (class:subtype? (instance-class obj) class))
     
;; ***********************************************************************
;; class:object-class : retourne la classe de l'instance obj
;; ***********************************************************************
(define (class:object-class obj)
  (instance-class obj))



(define (class:make-slot name init-val type)
  (if (instance? init-val)		; init-val != $non-initialized
      (error:check-type init-val type))
  (make-binding name init-val type #f))

(define (class:generic-slot-get slot-name slots)
  (let ((x (vassoc slot-name slots)))
    (if x
	(let ((y (binding-value x)))
	  (if (eq? y $non-initialized)
	      (dylan:error "Slot is not initialized: %=" 
			   (make-instance <symbol> slot-name))
	      y))
	(dylan:error "No such slot: %=" 
		     (make-instance <symbol> slot-name)))))

(define (class:generic-slot-set! slot-name val slots)
  (let ((x (vassoc slot-name slots)))
    (if x
	; on teste pour savoir si val est une instance générale du 
	; type de la slot.
	(begin
	  (error:check-type val (binding-type x))
	  (binding-value-set! x val))
	(dylan:error "No such slot: %=" 
		     (make-instance <symbol> slot-name)))))

(define (class:slot-get inst slot-name) 
  (class:generic-slot-get slot-name (instance-slots inst)))
(define (class:class-slot-get class slot-name)
  (class:generic-slot-get slot-name (class-class-slots (instance-slots class))))
(define (class:slot-set! inst slot-name val)
  (class:generic-slot-set! slot-name val (instance-slots inst)))
(define (class:class-slot-set! class slot-name val)
  (class:generic-slot-set! slot-name val (class-class-slots (instance-slots class))))
 
(define (class:make-getter name)
  (lambda (args)
    (let ((inst (car args)))
      (class:slot-get inst name))))

(define (class:make-setter name)
  (lambda (args)
    (let ((new-val (car args))
	  (inst    (cadr args)))
      (class:slot-set! inst name new-val)
      new-val)))

(define (class:make-class-getter alloc class name)
  (if (eq? alloc 'CLASS)
      (lambda (args)
	(let ((inst (car args)))
	  (class:class-slot-get class name)))
      (lambda (args)
	(let ((inst (car args)))
	  (class:class-slot-get (instance-class inst) name)))))

(define (class:make-class-setter alloc class name)
  (if (eq? alloc 'CLASS)
      (lambda (args)
	(let ((new-val (car args))
	      (inst    (cadr args)))
	  (class:class-slot-set! class name new-val)
	  new-val))
      (lambda (args)
	(let ((new-val (car args))
	      (inst    (cadr args)))
	  (class:class-slot-set! (instance-class inst) name new-val)
	  new-val))))


;; ***********************************************************************
;; class:add-methods! : ajoute les méthodes `getter' et `setter' des slots
;;   définies dans la classe.
;; ***********************************************************************
(define (class:add-methods! cl)
  (let loop ((l (class-slots (instance-slots cl))))
    (if (pair? l)
	(let* ((slot (car l))
	       (slot-descr (cdr slot))
	       (sl-class   (slot-class-def slot-descr))
	       (sl-type    (slot-type slot-descr))
	       (sl-alloc   (slot-allocation slot-descr))
	       (sl-getter  (slot-getter-name slot-descr))
	       (sl-setter  (slot-setter-name slot-descr)))
	  (if (and (eq? sl-class cl)
		   (or (eq? sl-alloc 'SUBCLASS)
		       (eq? sl-alloc 'CLASS)
		       (eq? sl-alloc 'INSTANCE)
		       (eq? sl-alloc 'CONSTANT)))
	      (begin
		(let ((gf (fun:ensure-generic-function sl-getter)))
		  (fun:add-method! 
		   gf
		   (fun:make-predef
		    sl-getter
		    (list (list 'instance cl))
		    #f
		    '()
		    #f
		    (make-return-value (list (list 'result sl-type)) #f #f)
		    (if (or (eq? sl-alloc 'INSTANCE)
			    (eq? sl-alloc 'CONSTANT))
			(class:make-getter sl-getter)
			(class:make-class-getter sl-alloc sl-class sl-getter)))))
		(if (not (eq? sl-setter $the-false-value))
		    (let ((gf (fun:ensure-generic-function sl-setter)))
		      (fun:add-method!
		       gf
		       (fun:make-predef
			sl-setter
			(list (list 'new-val sl-type) (list 'instance cl))
			#f
			'()
			#f
			(make-return-value (list (list 'result <object>)) #f #f)
			(if (or (eq? sl-alloc 'INSTANCE)
				(eq? sl-alloc 'CONSTANT))
			    (class:make-setter sl-getter)
			    (class:make-class-setter sl-alloc sl-class sl-getter))))))))
	  (loop (cdr l))))))
	    

;; ***********************************************************************
;; class:make : création d'une instance d'une classe
;; ***********************************************************************
(define (class:make class . keys)
  (define *no-instance?* 
    (not (class-instantiated? (instance-slots class))))
  
  (define (rest->pairs l)
    (if (null? l)
	'()
	(let ((key-name (instance-slots (car l)))
	      (key-val  (cadr l)))
	  (cons (cons key-name key-val) (rest->pairs (cddr l))))))
  
  (define (get-key key keys required? default)
    (let loop ((l keys))
      (if (null? l)
	  (if required? 
	      (dylan:error "Required init. keyword not specified in call to `make'")
	      default)
	  (if (eq? (instance-slots (car l)) key)
	      (cadr l)
	      (loop (cddr l))))))
	   
  (define (defaulted-init-args class keys)
    (let loop ((l (class-init-args (instance-slots class)))
	       (new-args '()))
      (if (null? l)
	  (append new-args keys)
	  (let* ((arg (car l))
		 (arg-name (car arg))
		 (arg-descr (cdr arg))
		 (type (init-arg-type arg-descr))
		 (x (get-key arg-name keys #f #f)))
	    (if (and (init-arg-required? arg-descr) (not x))
		(dylan:error "Required init. arg. not present: %=" 
			     (make-instance <symbol> arg-name)))
	    (if x
		(begin
		  (error:check-type x type)
		  (loop (cdr l) new-args))
		(let* ((initv (init-arg-init-value arg-descr))
		       (initf (init-arg-init-function arg-descr))
		       (val (if initv 
				initv
				(if initf
				    (fun:apply initf '() #f)
				    #f))))
		  (if val
		      (begin
			(error:check-type val type)
			(loop (cdr l) (cons (make-instance <symbol> arg-name)
					    (cons val
						  new-args))))
		      (loop (cdr l) new-args))))))))
		
  
  (define (init-class-slot! class name type initv initf keywd reqkey keys)
    (let* ((slots (class-class-slots (instance-slots class)))
	   (slot  (let ((s (vassoc name slots))) 
		    (if s 
			s 
			(let ((s2 (class:make-slot name #f type)))
			  (class-class-slots-set! 
			   (instance-slots class)
			   (cons s2 slots))
			  s2))))
	   (slot-val (binding-value slot)))
      (cond
       (reqkey
	(class:class-slot-set!
	 class name (get-key reqkey keys #t #f)))
       ((and *no-instance?* initv)
	(class:class-slot-set!
	 class name (if keywd (get-key keywd keys #f initv) initv)))
       ((and *no-instance?* initf)
	(class:class-slot-set!
	 class name
	 (if keywd
	     (let ((v (get-key keywd keys #f #f)))
	       (if v
		   v
		   (fun:apply initf '() #f)))
	     (fun:apply initf '() #f))))
       (keywd
	(class:class-slot-set! class name (get-key keywd keys #f slot-val)))
       (*no-instance?*
	(class:class-slot-set! class name $non-initialized)))))

  (define (alloc-slots keys)
    (let loop ((cl-slots (class-slots (instance-slots class))))
      (if (null? cl-slots)
	  '()
	  (let* ((slot (car cl-slots))
		 (slot-name  (car slot))
		 (slot-descr (cdr slot))
		 (slot-alloc (slot-allocation slot-descr))
		 (type (slot-type slot-descr))
		 (initv (slot-init-value slot-descr))
		 (initf (slot-init-function slot-descr))
		 (keywd (slot-init-key slot-descr))
		 (reqkey (slot-req-init-key slot-descr)))
	    (cond
	     ((or (eq? slot-alloc 'INSTANCE)
		  (eq? slot-alloc 'CONSTANT))
	      (cons
	       (class:make-slot 
		slot-name 
		(cond
		 (reqkey (get-key reqkey keys #t #f))
		 (initv  (if keywd 
			     (get-key keywd keys #f initv)
			     initv))
		 (initf  (if keywd
			     (let ((v (get-key keywd keys #f #f)))
			       (if v
				   v
				   (fun:apply initf '() #f)))
			     (fun:apply initf '() #f)))
		 (keywd  (get-key keywd keys #f $non-initialized))
		 (else   $non-initialized))
		type)
	       (loop (cdr cl-slots))))
	     ((eq? slot-alloc 'SUBCLASS)
	      (init-class-slot! class slot-name type initv initf keywd reqkey keys)
	      (loop (cdr cl-slots)))
	     ((eq? slot-alloc 'CLASS)
	      (init-class-slot! 
	       (slot-class-def slot-descr) slot-name type initv initf keywd reqkey keys)
	      (loop (cdr cl-slots)))
	     (else
	      (loop (cdr cl-slots))))))))
  
  (if (class:subtype? class <type>)
      (dylan:error "Default `make' method cannot create new types or classes"))
  (if (class-abstract? (instance-slots class))
      (dylan:error "Cannot instantiate an abstract class"))
  
  (if *no-instance?*
      (class-instantiated?-set! (instance-slots class) #t))

  (let* ((def-args (defaulted-init-args class keys)) 
	 (inst     (make-instance class (alloc-slots def-args))))
    ;; Call the `initialize' generic function
    (fun:apply 
     initialize-gf
     (cons inst def-args) 
     '()) 
    inst))