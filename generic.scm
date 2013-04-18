;; ---------------------------------------------------------------------- ;;
;; FICHIER               : generic.scm                                    ;;
;; DATE DE CREATION      : Mon May 29 09:43:57 1995                       ;;
;; DERNIERE MODIFICATION : Fri Jun  2 11:32:12 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; This file contains all the stuff for the creation and manipulation of  ;;
;; generic functions and methods (including the dispatch algorithm).      ;;
;; ---------------------------------------------------------------------- ;;


;; ***********************************************************************
;; Procédure s'assurant de l'existence d'une fonction générique
;; ***********************************************************************
(define (fun:ensure-generic-function name)
  (let ((x (vassoc name *global-environment*)))
    (if (or (not x) (eq? (binding-value x) $unbound))
	(let ((gf (make-instance
		   <generic-function>
		   (make-generic name -1 '() #f #f #f #f '() #f))))
	  (generic-code-set! (instance-slots gf)
			     (lambda (args)
			       (fun:apply-generic gf args)))
	  (if x
	      (begin
		(binding-value-set! x gf)
		(binding-type-set! x <generic-function>)
		(binding-constant?-set! x #t))
	      (env:add-global-env!
	       (env:make-binding name <generic-function> gf #t)))
	  gf)
	(let ((val (binding-value x)))
	  (if (generic? (instance-slots val))
	      val
	      (dylan:error "Object is not a generic function: %=" 
			   (make-instance <symbol> name)))))))

;; ***********************************************************************
;; fun:add-method! : Ajoute une méthode à une fonction générique
;; ***********************************************************************
(define (fun:add-method! gf method)
  (define the-method (instance-slots method))
  (define the-gf     (instance-slots gf))
  
  (define (make-list n val) 
    (if (= n 0) '() (cons val (make-list (- n 1) val))))
  
  (define (init-gf!)
    (let ((meth-nreq (method-nreq the-method))
	  (meth-rest (method-rest? the-method))
	  (meth-keys (method-keys the-method))
	  (meth-all? (method-all-keys? the-method))
	  (meth-val  (method-values the-method)))
      (generic-nreq-set! the-gf meth-nreq)
      (generic-spec-set! the-gf (make-list meth-nreq <object>))
      (generic-all-keys?-set! the-gf meth-all?) 
      (generic-keys-set! the-gf (if (or (pair? meth-keys) meth-all?) '() #f))
      (generic-rest?-set! the-gf (or (pair? meth-keys) meth-all? meth-rest))
;; ----- À modifier
      (generic-values-set! the-gf meth-val)))
;; -----
  
  (define (congruent-spec? gf-l meth-l)
    (or (null? gf-l)
	(and (let* ((gs (car gf-l)) (ms (cadar meth-l)))
	       (or (and (eq? <singleton> (instance-class ms))
			(class:general-instance? (instance-slots ms) gs))
		   (class:subtype? ms gs)))
	     (congruent-spec? (cdr gf-l) (cdr meth-l)))))
  
  (define (check-mandat-keys! gf-l meth-l)
    (if (pair? gf-l)
	(let* ((key (caar gf-l))
	       (the-key (if (keyword? key) (keyword-name key) key)))
	  (if (assq the-key meth-l)
	      (check-mandat-keys! (cdr gf-l) meth-l)
	      (dylan:error "Mandatory keyword not recognized by method: %=" 
			   (make-instance <symbol> the-key))))))
  
  (define (check-congruency!)
    (or (= (method-nreq the-method) (generic-nreq the-gf))
	(dylan:error "add-method!: different number of required arguments"))
    (or (congruent-spec? (generic-spec the-gf) (method-spec the-method))
	(dylan:error "add-method!: non-congruent specializers"))
    (cond
     ((generic-keys the-gf)
      (check-mandat-keys! (generic-keys the-gf) (method-keys the-method)))
     ((generic-rest? the-gf) 
      (if (not (and (method-rest? the-method) (null? (method-keys the-method))))
	  (dylan:error 
	   "add-method!: gf accepts a variable number of args, but method doesn't")))
     ((pair? (method-keys the-method))
      (dylan:error "add-method!: gf does not accept keys, but method does")))

    (and (method-all-keys? the-method)
	 (or (generic-all-keys? the-gf)
	     (dylan:error "add-method!: method accepts all keys, but gf doesn't"))))
	  
  (define (same-spec? method old-method)
    (let loop ((sp1 (method-spec method)) (sp2 (method-spec old-method)))
      (or (null? sp1)
	  (and (predef:== (cadar sp1) (cadar sp2))
	       (loop (cdr sp1) (cdr sp2))))))
  
  (if (= (generic-nreq the-gf) -1)
      (init-gf!)
      (check-congruency!))
  
  (let loop ((ml (generic-methods the-gf)))
    (if (null? ml) 
	(begin
	  (generic-methods-set! the-gf (cons method (generic-methods the-gf)))
	  (list method $the-false-value))
	(let ((m (car ml)))
	  (if (same-spec? the-method (instance-slots m))
	      (begin
		(set-car! ml method)
		(list method m))
	      (loop (cdr ml)))))))


;; ***********************************************************************
;; Procédure de création d'une fonction prédéfinie.
;; ***********************************************************************
(define (fun:make-predef name spec rest? keys all-keys? values proc)
  (make-instance
   <compiled-method>
   (make-method name -1 (length spec) spec rest? keys all-keys? values #f '() proc)))

;; ***********************************************************************
;; Application d'une fonction.
;; ***********************************************************************
(define (fun:apply fun args rte)
  (let ((fun-class (instance-class fun)))
    (cond
     ((eq? fun-class <generic-function>)
      ((generic-code (instance-slots fun)) args))
     
     ((eq? fun-class <compiled-method>)
      (let ((method (instance-slots fun)))
	(if (and (fun:applicable-method? fun args)
		 (fun:keyword-check method args))
	    (fun:apply-predef method args)
	    (dylan:error "Method is not applicable: %=" 
			 (make-instance <symbol> (method-name method))))))
     ((eq? fun-class <user-method>)
      (let ((method (instance-slots fun)))
	(if (and (fun:applicable-method? fun args)
		 (fun:keyword-check method args))
	    (fun:apply-method fun args $the-false-value)
	    (dylan:error "Method is not applicable: %=" 
			 (make-instance <symbol> (method-name method))))))
     ((eq? fun-class <next-method>)
      ((method-code (instance-slots fun)) args rte))
     (else
      (dylan:error "Illegal function call")))))


;; ***********************************************************************
;; fun:apply-generic
;; ***********************************************************************
(define (fun:apply-generic fun args)
  (define (firsts n l)
    (if (eq? n 0) '() (cons (car l) (firsts (- n 1) (cdr l)))))
  
  (let* ((applicable-methods (filter (lambda (m)
				       (fun:applicable-method? m args))
				    (generic-methods (instance-slots fun)))))
    (cond
     ((null? applicable-methods)
      (dylan:error "No applicable method for: %=" 
		   (make-instance <symbol> (generic-name (instance-slots fun)))))
     (else
      (let* ((no-actuals (length (generic-spec (instance-slots fun))))
	     (actuals (firsts no-actuals args))
	     (sorted-methods (car (fun:sort-methods applicable-methods actuals))))
	(if (null? sorted-methods)
	    (dylan:error "Ambiguous method: %=" 
			 (make-instance <symbol> (generic-name (instance-slots fun))))
	    (let ((meth (car sorted-methods))
		  (nextl (cdr sorted-methods)))
	      (if (eq? <compiled-method> (instance-class meth))
		  (fun:apply-predef (instance-slots meth) args)
		  (fun:apply-method
		   meth 
		   args 
		   (if (null? nextl) $the-false-value nextl) 
		  )))))))))

;; ***********************************************************************
;; Application d'une fonction prédéfinie.
;; ***********************************************************************
(define (fun:apply-predef fun args)
  (let ((code (method-code fun)))
    (code args)))


(define-macro (add-binding rte i name type val)
  `(vector-set! ,rte ,i (make-binding ,name ,val ,type #F)))

;; ***********************************************************************
;; Application d'une méthode (créée à l'exécution). 
;; NOTE : le nombre d'arguments et leur types ont déjà été vérifiés.
;; ***********************************************************************
(define (fun:apply-method fun args nextl)
  (define (extend/formals formals actuals rte)
    (let loop ((i 1) (actuals actuals) (formals formals))
      (if (null? formals)
	  i
	  (let* ((form     (car formals))
		 (arg-name (car form))
		 (arg-type (cadr form)))
	    (add-binding 
	     rte i arg-name 
	     (if (eq? <singleton> (instance-class arg-type))
		 (instance-class (instance-slots arg-type))
		 arg-type)
	     (car actuals))
	    (loop (+ i 1) (cdr actuals) (cdr formals))))))
  
  (define (rest->pairs l)
    (if (null? l)
	'()
	(let ((key-name (instance-slots (car l)))
	      (key-val  (cadr l)))
	  (cons (cons key-name key-val) (rest->pairs (cddr l))))))
  
  (define (extend/keys keys pairs rte rtep)
    (let loop ((i rtep) (keys keys))
      (if (pair? keys)
	  (let* ((key (car keys))
		 (key-name (car key))
		 (key-var (cadr key))
		 (key-def (caddr key))
		 (provided (assq key-name pairs)))
	    (add-binding rte i key-var <object> 
			 (if provided (cdr provided) (key-def rte)))
	    (loop (+ i 1) (cdr keys))))))
     
  (let* ((meth    (instance-slots fun))
	 (formals (method-spec meth))
	 (rte     (method-env meth))
	 (fs      (method-fs  meth))
	 (code    (method-code meth))
	 (rest?   (method-rest? meth))
	 (keys    (method-keys meth))
	 (next    (method-next-name meth))
	 (rest    (list-tail args (method-nreq meth))))
    
    (let* ((new-rte (link rte fs))
	   (rtep1   (extend/formals formals args new-rte))
	   (rtep2   (if next
			(begin
			  (add-binding new-rte rtep1 next <object> 
				       (fun:make-next-method args nextl))
			  (+ rtep1 1))
			rtep1))
	   (rtep3 (if rest?
		     (begin
		       (add-binding new-rte rtep2 rest? <sequence> 
				    (predef:make-list rest))
		       (+ rtep2 1))
		     rtep2)))
      (if (pair? keys)
	  (extend/keys keys (rest->pairs rest) new-rte rtep3))
      (code new-rte))))


;; ***********************************************************************
;; Fonction de création d'une méthode `next'.
;; ***********************************************************************
(define (fun:make-next-method args nextl)
  (let ((code (lambda (suppl-args rte)
		(if (eq? nextl $the-false-value)
		    (dylan:error "Ambiguous or no next method"))
		(let ((fun (car nextl))
		      (nextl-rest (let ((x (cdr nextl))) 
				    (if (null? x) $the-false-value x)))
		      (new-args (if (null? suppl-args) args suppl-args)))
		  (if (eq? (instance-class fun) <compiled-method>)
		      (fun:apply-predef (instance-slots fun) new-args)
		      (fun:apply-method fun args nextl-rest))))))
    (make-instance
     <next-method>
     (make-method 'anonymous 1 -1 '() #f #f #f #f #f '() code))))

;; ***********************************************************************
;; Vérification des mots-clé fournis à l'appel d'une fonction.
;; NOTE: On suppose que la méthode est applicable 
;; (bon nombre d'arguments fournis).
;; ***********************************************************************
(define (fun:keyword-check fun args)
  (let* ((nreq  (method-nreq fun))
	 (rest  (list-tail args nreq))
	 (keys  (method-keys fun))
	 (all?  (method-all-keys? fun))
	 (rest? (method-rest? fun)))

    ;; S'il y a des arguments supplémentaires et que la méthode
    ;; requiert seulement un nombre fixe de paramètres ...
    (if (and (null? keys) (not all?) (not rest?) (> (length rest) 0))
	(dylan:error "Too many arguments passed to method"))
    
    ;; Si la méthode accepte des mots-clé, on vérifie que les arguments
    ;; supplémentaires sont des paires clé/valeur;
    ;; et si la méthode n'accepte pas tous les mots-clé, on vérifie que ceux 
    ;; qui sont fournis sont acceptés ...
    (if (or (pair? keys) all?)
	(let ((not-all (and (not all?) rest)))
	  (let loop ((l rest))
	    (if (pair? l)
		(let ((key (car l)))
		  (if (or (not (eq? (instance-class key) <symbol>))
			  (null? (cdr l)))
		      (dylan:error "Invalid key/value pair for: `%=:'" 
				   (make-instance <symbol> key))
		      (if not-all
			  (let* ((name (instance-slots key))
				 (x (assq name keys)))
			    (if (not x)
				(dylan:error "Method does not recognize keyword: `%=:'" 
					     (make-instance <symbol> name))
				(loop (cddr l))))
			  #t)))
		#t)))
	#t)))


    

;; ***********************************************************************
;; Fonction qui détermine si une méthode est applicable.
;; ***********************************************************************
(define (fun:applicable-method? fun args)
  (let* ((meth (instance-slots fun))
	 (nsupplied (length args))
	 (nreq      (method-nreq meth)))
    (cond
     ((< nsupplied nreq)
      #f)
     ((and (not (method-rest? meth)) 
	   (or (not (method-keys meth)) 
	       (null? (method-keys meth)))
	   (not (method-all-keys? meth))
	   (> nsupplied nreq))
      #f)
     (else
      (let loop ((formals (method-spec meth))
		 (actuals args))
	(if (null? formals)
	    #t
	    (let* ((form (car formals))
		   (form-type (cadr form))
		   (arg  (car actuals)))
	      (if (eq? (instance-class form-type) <singleton>)
		  (if (predef:== (instance-slots form-type) arg)
		      (loop (cdr formals) (cdr actuals))
		      #f)
		  (if (class:general-instance? arg form-type)
		      (loop (cdr formals) (cdr actuals))
		      #f)))))))))
	 

;; ***********************************************************************
;; fun:sort-methods : trie les methodes applicables et retourne
;; deux listes. 
;; ***********************************************************************
(define (fun:sort-methods methods actuals)
  (define (pos-in-list elt lst)
    (let loop ((pos 0) (l lst))
      (if (null? l) 
	  -1 
	  (if (eq? elt (car l))
	      pos
	      (loop (+ pos 1) (cdr l))))))
    
  (define (arg-specif spec1 spec2 actual)
    (let* ((typ1 (cadr spec1)) 
	   (typ2 (cadr spec2))
	   (cl1 (instance-class typ1))
	   (cl2 (instance-class typ2)))
      (cond
       ((eq? typ1 typ2)
	'=)
       ((eq? cl1 <singleton>)
	(if (eq? cl2 <singleton>)
	    (if (eq? (instance-slots typ1) (instance-slots typ2))
		'=
		'<>)
	    (if (class:general-instance? (instance-slots typ1) typ2)
		'<
		'<>)))
       ((eq? cl2 <singleton>)
	(if (class:general-instance? (instance-slots typ2) typ1)
	    '>
	    '<>))
       ((and
	 (eq? <class> (instance-class typ1))
	 (eq? <class> (instance-class typ2)))
	(let* ((pl (class-prec-list (instance-slots (instance-class actual))))
	       (pos1 (pos-in-list typ1 pl))
	       (pos2 (pos-in-list typ2 pl)))
	  (if (and (>= pos1 0) (>= pos2 0))
	      (if (< pos1 pos2)
		  '<
		  '>)
	      '<>)))
       (else '<>))))
  
  (define (method-specif meth1 meth2 args)
    (let loop ((arg-specs (map arg-specif 
			       (method-spec (instance-slots meth1))
			       (method-spec (instance-slots meth2))
			       args))
	       (meth-spec '=))
      (if (null? arg-specs)
	  meth-spec
	  (let ((arg-spec (car arg-specs)))
	    (if (or (and (eq? meth-spec '>)
			 (eq? arg-spec '<))
		    (and (eq? meth-spec '<)
			 (eq? arg-spec '>)))
		'<>
		(if (eq? meth-spec '=)
		    (loop (cdr arg-specs) arg-spec)
		    (loop (cdr arg-specs) meth-spec)))))))
  
  (define (split-before lst pos)
    (if (= -1 pos) 
	(list lst '())
	(let ((n (length lst)))
	  (list (reverse (list-tail (reverse lst) (- n pos)))
		(list-tail lst pos)))))
  

  (define *ambig-pos* -1)
  
  (define (add-method meth sorted-lst args pos)
    (if (null? sorted-lst)
	(list meth)
	(let* ((first (car sorted-lst))
	       (spec  (method-specif meth first args)))
	  (cond
	   ((eq? spec '<)
	    (if (<= pos *ambig-pos*)
		(set! *ambig-pos* (+ *ambig-pos* 1)))
	    (cons meth sorted-lst))
	   ((eq? spec '>)
	    (cons first (add-method meth (cdr sorted-lst) args (+ pos 1))))
	   ((eq? spec '<>)
	    (if (or (= *ambig-pos* -1)
		    (< pos *ambig-pos*))
		(set! *ambig-pos* pos))
	    (cons meth sorted-lst))))))
	    
  
  (let loop ((sorted-methods '()) (meth-lst methods))
    (if (null? meth-lst)
	(split-before sorted-methods *ambig-pos*)
	(let ((meth (car meth-lst)))
	  (loop (add-method meth sorted-methods actuals 0) (cdr meth-lst))))))

	
	 
