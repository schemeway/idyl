;; ---------------------------------------------------------------------- ;;
;; FICHIER               : gen.scm                                        ;;
;; DATE DE CREATION      : Mon May 29 09:41:19 1995                       ;;
;; DERNIERE MODIFICATION : Fri Jun  9 10:54:15 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; This file contains the intermediate code generator (based on closure   ;;
;; generation).                                                           ;;
;; ---------------------------------------------------------------------- ;;

;; ---------------------------------------------------------------------- ;;
;; get-first-val : extrait la première valeur d'une liste de résultats.   ;;
;; ---------------------------------------------------------------------- ;;
(define-macro (get-first-val vals)
  `(LET ((V ,vals))
     (IF (NULL? V) $THE-FALSE-VALUE 
	 (IF (PAIR? V) (CAR V) V))))

(define-macro (empty-cte) ''())

;; ---------------------------------------------------------------------- ;;
;; Déclaration des procédure de compilation des expressions `top-level'.  ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-constituent c)
  (cond
   ((defining-form? c)
    (let ((form (defining-form-type c)))
      (cond
       ((def-variable? form)
	(gen:def-var form))
       ((def-constant? form)
	(gen:def-const form))
       ((def-class? form)
	(gen:def-class form))
       ((def-method? form)
	(gen:def-method form))
       ((def-generic? form)
	(gen:def-generic form))
       ((module-defn? form)
	(dylan:warning "Module declarations not handled")
	(gen:gen-cst $the-false-value))
       (else
	(dylan:error "Unhandled defining form")))))
   
   ((expr? c)
    (let ((e (expr-expr c)))
      (if (and (statement? e) (begin-form? (statement-statement e)))
	  (gen:gen-top-level (begin-form-body (statement-statement e)))
	  (gen:gen-expr (empty-cte) (expr-expr c)))))
   
   ((local-decl? c)
    (dylan:error "Local declaration cannot appear at toplevel"))))

(define (gen:gen-top-level body)
  (cond
   ((null? body)
    (gen:gen-cst $the-false-value))
   ((and (pair? body) (null? (cdr body)))
    (gen:gen-constituent (car body)))
   (else
    (let* ((first (car body))
	   (comp-first (gen:gen-constituent first))
	   (comp-rest  (gen:gen-top-level (cdr body))))
      (lambda (rte)
	(comp-first rte)
	(comp-rest rte))))))

    
;; ---------------------------------------------------------------------- ;;
;; Déclaration des procédure de compilation des formes définissantes.     ;;
;; ---------------------------------------------------------------------- ;;

;; --- Ajoute une variable globale et retourne la liaison ... ----------- ;;
(define (ensure-module-var name)
  (let ((x (vassoc name *global-environment*)))
    (if x 
	(if (eq? (binding-value x) $unbound)
	    x
	    (begin
	      (dylan:warning "redefinition of module variable: %="
			     (predef:make-symbol name))
	      x))
	(let ((y (make-binding name $unbound $unbound #f)))
	  (env:add-global-env! y)
	  y))))

;; --- parours une liste de variables (+ #rest) ------------------------- ;;
(define (gen-bindings cte decls)
  (let loop ((l decls))
    (if (null? l)
	'()
	(let ((formal (car l)))
	  (if (variable? formal)
	      ; variable 
	      (let* ((name      (variable-name formal))
		     (type      (variable-type formal))
		     (comp-type (if type 
				    (gen:gen-expr cte type)
				    (gen:gen-cst <object>)))
		     (bind      (ensure-module-var name)))
		(binding-type-set! bind (comp-type #f))
		(cons (cons bind #f) (loop (cdr l))))
	      ; paramètre `#REST'
	      (let* ((name (rest-param-name formal))
		     (bind (ensure-module-var name)))
		(binding-type-set! bind <object>)
		(list (cons bind #t)))))))) 

  
(define (add-globals bindings values constant?)
  (cond
   ((pair? bindings)
    (let* ((bind  (caar bindings))
	   (rest? (cdar bindings)))
      (if rest?
	  (begin
	    (env:set! (predef:make-list values) bind)
	    (binding-constant?-set! bind constant?))
	  (if (null? values)
	      (begin
		(env:set! $the-false-value bind)
		(binding-constant?-set! bind constant?)
		(add-globals (cdr bindings) values constant?))
	      (begin
		(env:set! (car values) bind)
		(binding-constant?-set! bind constant?)
		(add-globals (cdr bindings) (cdr values) constant?))))))
   (else #t)))

;; --- Les variables ---------------------------------------------------- ;;
(define (gen:def-var defn)
  (let* ((decls    (def-variable-names defn))
	 (bindings (gen-bindings (empty-cte) decls))
	 (values   (gen:gen-expr (empty-cte) (def-variable-value defn))))
    (lambda (rte)
      (let ((x (values rte)))
	(add-globals bindings (if (lst? x) x (list x)) #f)
	$the-false-value))))

;; --- Les constantes --------------------------------------------------- ;;
(define (gen:def-const defn)
  (let* ((decls    (def-constant-names defn))
	 (bindings (gen-bindings (empty-cte) decls))
	 (values   (gen:gen-expr (empty-cte) (def-constant-value defn))))
    (lambda (rte)
      (let ((x (values rte)))
	(add-globals bindings (if (lst? x) x (list x)) #t)
	$the-false-value))))

;; --- Les classes ------------------------------------------------------ ;;
(define (gen:def-class defn)
  (define (compile-gut g)
    (cond
     ((slot-spec? g)
      (let* ((name    (slot-spec-name g))
	     (flags   (slot-spec-flags g))
	     (alloc   (slot-spec-alloc g))
	     (sl-type (slot-spec-type g))
	     (type    (if sl-type
			  (gen:gen-expr (empty-cte) sl-type)
			  (lambda (rte) <object>)))
	     (keys    (map (lambda (p) (cons (keyword-name (car p))
					     (cdr p)))
			   (slot-spec-prop-list g))))
	(list 'SLOT name flags alloc type keys)))

     ((keyword-spec? g)
      (list 'KEYWORD 
	    (keyword-spec-name g) 
	    (keyword-spec-required? g)
	    (map (lambda (p) (cons (keyword-name (car p)) (cdr p)))
		 (keyword-spec-prop-list g))))

     ((inherited-spec? g)
      (list 'INHERITED-SLOT
	    (inherited-spec-name g)
	    (map (lambda (p) (cons (keyword-name (car p)) (cdr p)))
		 (inherited-spec-prop-list g))))))
  
  (let* ((flags       (def-class-flags defn))
	 (name        (def-class-name  defn))
	 (comp-supers (map (lambda (s)
			     (gen:gen-expr (empty-cte) s))
			   (def-class-super defn)))
	 (comp-guts   (map compile-gut  (def-class-guts  defn)))
	 (binding     (ensure-module-var name)))
    
    (binding-type-set! binding <class>)
    
    (lambda (rte)
      (let* ((supers (map (lambda (x) (x rte)) comp-supers))
	     (slots  comp-guts)
	     (class  (class:make-class name flags supers slots)))
	(env:set! class binding)
	(binding-constant?-set! binding #t)
	(class:adjust-slots! class rte)
	(class:add-methods! class)
	class))))


;; --- Les methodes ----------------------------------------------------- ;;
(define (gen:def-method form)
  (let* ((name (def-method-name form))
	 (comp-method (gen:gen-method 
		       (empty-cte)
		       name 
		       'next-method
		       (def-method-params form)
		       (def-method-return-type form)
		       (def-method-body form))))
    (lambda (rte)
      (let* ((method (comp-method rte))
	     (next?  (method-next-name (instance-slots method)))
	     (gf     (fun:ensure-generic-function name)))
	(fun:add-method! gf method)))))

;; --- Les fonctions generiques ----------------------------------------- ;;
(define (gen:def-generic form)
  (define (comp-req-params params)
    (map (lambda (param)
	   (let ((t (position-param-type param))
		 (s (position-param-singleton param)))
	     (cond
	      (t (gen:gen-expr (empty-cte) t))
	      (s (let ((singl (gen:gen-expr (empty-cte)
					    (position-param-singleton param))))
		   (lambda (rte)
		     (make-instance <singleton> (singl rte)))))
	      (else (gen:gen-cst <object>)))))
	 params))
  
  (define (comp-keywords gf-keys)
    (map (lambda (k)
	   (let ((type (let ((t (gf-keyword-type k)))
			 (if t 
			     (gen:gen-expr (empty-cte) t) 
			     (gen:gen-cst <object>)))))
	     (list (gf-keyword-keyword k) type)))
	 gf-keys))
  
  (let* ((name      (def-generic-name form))
	 (params    (def-generic-params form))
	 (values    (def-generic-return-type form))
	 (comp-req  (comp-req-params (filter (lambda (x) (position-param? x)) params)))
	 (rest?     (pair? (filter (lambda (x) (rest-param? x)) params)))
	 (all-keys? (pair? (filter (lambda (x) (all-keys? x)) params)))
	 (comp-keys (comp-keywords (filter (lambda (x) (gf-keyword? x)) params)))
	 (binding   (ensure-module-var name)))
    
    (binding-type-set! binding <generic-function>)

    (lambda (rte)
      (let* ((req (map (lambda (t)
			 (let ((type (t rte)))
			   (error:check-type type <type>)))
		       comp-req))
	     (keys (if comp-keys
		       (map (lambda (k)
			      (let ((t ((cadr k) rte)))
				(error:check-type t <type>)
				(list (car k) t)))
			    comp-keys)
		       #f))
	     (gf (make-instance 
		  <generic-function>
		  (make-generic 
		   name (length req) req rest? keys all-keys? values '() #f))))
	(generic-code-set! (instance-slots gf)
			   (lambda (args)
			     (fun:apply-generic gf args)))
	(env:set! gf binding)
	(binding-constant?-set! binding #t)
	gf))))
	  
;; ---------------------------------------------------------------------- ;;
;; Déclaration des procédures de compilation des expressions              ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-expr cte expr)
  (cond

;; --- Expressions constantes ------------------------------------------- ;;
   ((true-constant? expr)
    (gen:gen-cst $the-true-value))
   ((false-constant? expr)
    (gen:gen-cst $the-false-value))
   ((concat-string? expr)
    (gen:gen-concat-str cte(concat-string-list expr)))
   ((character? expr)
    (gen:gen-cst (predef:make-char (character-value expr))))
   ((ast-integer? expr)
    (gen:gen-cst (predef:make-int (ast-integer-value expr))))
   ((float? expr)
    (gen:gen-cst (predef:make-float (float-value expr))))
   ((literal-symbol? expr)
    (gen:gen-cst (predef:make-symbol (literal-symbol-name expr))))
   ((dotted-list? expr)
    (gen:gen-dotted-list cte (dotted-list-before expr) (dotted-list-after expr)))
   ((literal-list? expr)
    (gen:gen-list cte (literal-list-elements expr)))
   ((literal-vector? expr)
    (gen:gen-vector cte (literal-vector-elements expr)))
   ((keyword? expr)
    (gen:gen-cst (predef:make-symbol (keyword-name expr))))
   
;; --- Expressions non-constantes --------------------------------------- ;;
   ((ast-symbol? expr)
    (gen:gen-ref cte (ast-symbol-name expr)))
   ((slot-reference? expr)
    (gen:gen-slot-ref cte expr))
   ((array-ref? expr)
    (gen:gen-aref cte (array-ref-seq expr) (array-ref-element expr)))
   ((assignment? expr)
    (gen:gen-assign cte (assignment-place expr) (assignment-new-value expr)))
   ((or-expr? expr)
    (gen:gen-or cte (or-expr-form1 expr) (or-expr-form2 expr)))
   ((and-expr? expr)
    (gen:gen-and cte (and-expr-form1 expr) (and-expr-form2 expr)))
   ((unary-op? expr)
    (gen:gen-unary cte (unary-op-operator expr) (unary-op-operand expr)))
   ((funcall? expr)
    (gen:gen-funcall cte (funcall-function expr) (funcall-arguments expr)))
   ((anonym-method? expr)
    (let* ((descr (anonym-method-descr expr))
	   (params (car descr))
	   (values (cadr descr))
	   (body (caddr descr)))
      (gen:gen-method cte 'anonymous #f params values body)))
   
;; --- Enonces ---------------------------------------------------------- ;;
   ((statement? expr)
    (let ((st (statement-statement expr)))
      (cond
       ((begin-form? st)
	(gen:gen-body cte (begin-form-body st)))
       ((while? st)
	(gen:gen-while cte (while-test st) (while-body st)))
       ((until? st)
	(gen:gen-until cte (until-test st) (until-body st)))
       ((ast-if? st)
	(gen:gen-if cte (ast-if-test st) (ast-if-then-part st) (ast-if-else-part st)))
       ((ast-case? st)
	(gen:gen-case cte (ast-case-cond-body st)))
       ((select? st)
	(gen:gen-select cte (select-target-form st) (select-test st) (select-cases st)))
       ((block-exit? st)
	(gen:gen-block 
	 cte (block-exit-symbol st) (block-exit-body st) (block-exit-epilog st)))
       ((ast-for? st)
	(gen:gen-for cte(ast-for-header st) (ast-for-body st) (ast-for-final-part st)))
       (else
	(dylan:error "Unhandled statement")))))
   (else
    (pp expr)
    (dylan:error "Unhandled expression"))))


;; ---------------------------------------------------------------------- ;;
;; Les constantes ...                                                     ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-cst c) (lambda (rte) c))

;; ---------------------------------------------------------------------- ;;
;; Les chaînes de caractères (concaténation)                              ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-concat-str cte lst)
  (let loop ((s (car lst)) (l (cdr lst)))
    (if (null? l)
	(gen:gen-cst (predef:make-byte-string s))
	(loop (string-append s (car l)) (cdr l)))))

;; ---------------------------------------------------------------------- ;;
;; Les listes impropres ...                                               ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-dotted-list cte before after)
  (let ((comp-before (map (lambda (c) (gen:gen-expr cte c)) before))
	(comp-after  (gen:gen-expr cte after)))
    (gen:gen-cst
     (let loop ((l comp-before))
       (if (null? (cdr l))
	   (predef:pair ((car l) #f) (comp-after #f))
	   (predef:pair ((car l) #f)
		     (loop (cdr l))))))))

;; ---------------------------------------------------------------------- ;;
;; Les listes propres                                                     ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-list cte elts)
  (gen:gen-cst 
   (predef:make-list
    (let loop ((l elts))
      (if (null? l)
	  '()
	  (cons ((gen:gen-expr cte (car l)) #f)
		(loop (cdr l))))))))

;; ---------------------------------------------------------------------- ;;
;; Les vecteurs                                                           ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-vector cte elts)
  (gen:gen-cst 
   (predef:make-vector
    (let loop ((l elts))
      (if (null? l)
	  '()
	  (cons ((gen:gen-expr cte (car l)) #f)
		(loop (cdr l))))))))


(define-macro (add-binding rte i name type val)
  `(VECTOR-SET! ,rte ,i (MAKE-BINDING ,name ,val ,type #F)))


;; ---------------------------------------------------------------------- ;;
;; Trouve une variable dans l'environnement cte                           ;;
;; ---------------------------------------------------------------------- ;;
(define (find-up-and-over name cte)
  (let frame-loop ((l cte) (o 0))
    (if (null? l)
	#f
	(let binding-loop ((f (car l)) (u 1))
	  (if (null? f)
	      (frame-loop (cdr l) (+ o 1))
	      (let ((var (car f)))
		(if (eq? name var)
		    (cons o u)
		    (binding-loop (cdr f) (+ u 1)))))))))

;; ---------------------------------------------------------------------- ;;
;; Les références de variables                                            ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-ref cte name)
  (let ((up-and-over (find-up-and-over name cte)))
    (if up-and-over
	(let ((over (car up-and-over))
	      (up   (cdr up-and-over)))
;; --- Un peu de spécialisation sur le up-and-over ---------------------- ;;
	  (cond
	   ((= over 0)
	    (cond
	     ((= up 1)
	      (lambda (rte) (binding-value (vector-ref rte 1))))
	     ((= up 2)
	      (lambda (rte) (binding-value (vector-ref rte 2))))
	     ((= up 3)
	      (lambda (rte) (binding-value (vector-ref rte 3))))
	     (else
	      (lambda (rte) (binding-value (vector-ref rte up))))))
	   ((= over 1)
	    (cond
	     ((= up 1)
	      (lambda (rte) (binding-value (vector-ref (vector-ref rte 0) 1))))
	     ((= up 2)
	      (lambda (rte) (binding-value (vector-ref (vector-ref rte 0) 2))))
	     ((= up 3)
	      (lambda (rte) (binding-value (vector-ref (vector-ref rte 0) 3))))
	     (else
	      (lambda (rte) (binding-value (vector-ref (vector-ref rte 0) up))))))
	   ((= over 2)
	    (cond
	     ((= up 1)
	      (lambda (rte)
		(binding-value (vector-ref (vector-ref (vector-ref rte 0) 0) 1))))
	     ((= up 2)
	      (lambda (rte)
		(binding-value (vector-ref (vector-ref (vector-ref rte 0) 0) 2))))
	     ((= up 3)
	      (lambda (rte)
		(binding-value (vector-ref (vector-ref (vector-ref rte 0) 0) 3))))
	     (else
	      (lambda (rte)
		(binding-value (vector-ref (vector-ref (vector-ref rte 0) 0) up))))))
	   (else
	    (lambda (rte)
	      (let loop ((o over) (fp rte))
		(if (= o 0)
		    (binding-value (vector-ref fp up))
		    (loop (- o 1) (vector-ref fp 0))))))))
	
;; --- Variable globale ... --------------------------------------------- ;;
	(let* ((x    (vassoc name *global-environment*))
	       (bind (if x x (ensure-module-var name))))
	  (lambda (rte) 
	    (let ((val (binding-value bind)))
	      (if (eq? val $unbound)
		  (dylan:error "Unbound module variable: %="
			       (predef:make-symbol name))
		  val)))))))

;; ---------------------------------------------------------------------- ;;
;; Les affectations (de variable)                                         ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-set! cte name comp-val)
  (let ((up-and-over (find-up-and-over name cte)))
    (if up-and-over
	(let ((over (car up-and-over))
	      (up   (cdr up-and-over)))
	  (cond
	   ((= over 0)
	    (cond
	     ((= up 1)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref rte 1))
		  new-val)))
	     ((= up 2)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref rte 2))
		  new-val)))
	     ((= up 3)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref rte 3))
		  new-val)))
	     (else
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref rte up))
		  new-val)))))
	   ((= over 1)
	    (cond
	     ((= up 1)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref rte 0) 1))
		  new-val)))
	     ((= up 2)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref rte 0) 2))
		  new-val)))
	     ((= up 3)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref rte 0) 3))
		  new-val)))
	     (else
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref rte 0) up))
		  new-val)))))
	   ((= over 2)
	    (cond
	     ((= up 1)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref (vector-ref rte 0) 0) 1))
		  new-val)))
	     ((= up 2)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref (vector-ref rte 0) 0) 2))
		  new-val)))
	     ((= up 3)
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref (vector-ref rte 0) 0) 3))
		  new-val)))
	     (else
	      (lambda (rte) 
		(let ((new-val (comp-val rte)))
		  (env:set! new-val (vector-ref (vector-ref (vector-ref rte 0) 0) up))
		  new-val)))))
	   (else
	    (lambda (rte)
	      (let ((new-val (comp-val rte)))
		(let loop ((o over) (fp rte))
		  (if (= o 0)
		      (begin
			(env:set! new-val (vector-ref fp up))
			new-val)
		      (loop (- o 1) (vector-ref fp 0)))))))))
	
;; --- affectation à une variable globale ... --------------------------- ;;
	(let* ((x    (vassoc name *global-environment*))
	       (bind (if x x (ensure-module-var name))))
	  (lambda (rte) 
	    (let ((val (binding-value bind)))
	      (if (eq? val $unbound)
		  (dylan:error "Undefined module variable: %="
			       (predef:make-symbol name))
		  (let ((new-val (comp-val rte)))
		    (env:set! new-val bind)
		    new-val))))))))

;; ---------------------------------------------------------------------- ;;
;; Les références à des champs (slots)                                    ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-slot-ref cte expr)
  (let ((comp-fun (gen:gen-ref cte (slot-reference-slot expr)))
	(comp-arg (gen:gen-expr cte (slot-reference-object expr))))
    (lambda (rte)
      (let ((arg (comp-arg rte)) (fun (comp-fun rte)))
	(fun:apply fun (list (get-first-val arg)) rte)))))


;; ---------------------------------------------------------------------- ;;
;; Les références de tableaux ...                                         ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-aref cte seq elts)
  (let ((aref-fun (if (> (length elts) 1) 'aref 'element)))
    (gen:gen-funcall cte (make-ast-symbol aref-fun) (cons seq elts))))

;; ---------------------------------------------------------------------- ;;
;; Les affectations (syntaxes spéciales)                                  ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-assign cte place new-val)
  (cond
;; --- var := new-val --------------------------------------------------- ;;
   ((ast-symbol? place)
    (let ((comp-val (gen:gen-expr cte new-val)))
      (gen:gen-set! cte (ast-symbol-name place) comp-val)))
;; --- fun (x1, ...) := new-val ----------------------------------------- ;;
   ((and (funcall? place) (ast-symbol? (funcall-function place)))
    (let* ((sym      (funcall-function place))
	   (new-name (dylan:string->symbol
		      (string-append
		       (symbol->string (ast-symbol-name sym))
		       "-setter"))))
      (ast-symbol-name-set! sym new-name)
      (gen:gen-funcall cte sym (cons new-val (funcall-arguments place)))))
;; --- seq[x1, ...] := new-val ------------------------------------------ ;;
   ((array-ref? place)
    (let* ((indices    (array-ref-element place))
	   (setter-fun (if (> (length indices) 1) 'aref-setter 'element-setter)))
      (gen:gen-funcall 
       cte
       (make-ast-symbol setter-fun)
       (cons new-val (cons (array-ref-seq place) (array-ref-element place))))))
;; --- obj.slot := new-val ---------------------------------------------- ;;
   ((slot-reference? place)
    (let ((comp-fun (gen:gen-ref cte
				 (dylan:string->symbol 
				  (string-append 
				   (symbol->string (slot-reference-slot place))
				   "-setter"))))
	  (comp-arg (gen:gen-expr cte (slot-reference-object place)))
	  (comp-val (gen:gen-expr cte new-val)))
      (lambda (rte)
	(let ((val (get-first-val (comp-val rte)))
	      (arg (get-first-val (comp-arg rte))))
	(fun:apply (comp-fun rte) (list val arg) rte)))))
;; --- ???? ------------------------------------------------------------- ;;
   (else
    (dylan:error "Illegal assignment form"))))

;; ---------------------------------------------------------------------- ;;
;; Les formes OR                                                          ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-or cte form1 form2)
  (let ((comp-val1 (gen:gen-expr cte form1))
	(comp-val2 (gen:gen-expr cte form2)))
    (lambda (rte)
      (let* ((val       (comp-val1 rte))
	     (first-val (get-first-val val)))
	(if (eq? first-val $the-false-value)
	    (comp-val2 rte)
	    first-val)))))

;; ---------------------------------------------------------------------- ;;
;; Les formes AND                                                         ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-and cte form1 form2)
  (let ((comp-val1 (gen:gen-expr cte form1))
	(comp-val2 (gen:gen-expr cte form2)))
    (lambda (rte)
      (let* ((val       (comp-val1 rte))
	     (first-val (get-first-val val)))
	(if (eq? first-val $the-false-value)
	    first-val
	    (comp-val2 rte))))))

;; ---------------------------------------------------------------------- ;;
;; Les operateurs unaires (~ et -)                                        ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-unary cte rator rand)
  (let ((comp-rand (gen:gen-expr cte rand)))
    (cond 
     ((eq? rator '-)
      (let ((comp-neg (gen:gen-ref cte 'negative)))
	(lambda (rte)
	  (fun:apply 
	   (comp-neg rte) (list (get-first-val (comp-rand rte))) rte))))
     ((eq? rator '~)
      (lambda (rte)
	(let ((v (get-first-val (comp-rand rte))))
	  (if (eq? v $the-false-value)
	      $the-true-value
	      $the-false-value)))))))

;; ---------------------------------------------------------------------- ;;
;; Les appels de fonctions                                                ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-funcall cte fun args)
  (let ((comp-fun (gen:gen-expr cte fun))
	(comp-args (map (lambda (arg) (gen:gen-expr cte  arg)) args)))
    (lambda (rte)
      (let ((fun  (get-first-val (comp-fun rte)))
	    (args (let loop ((l comp-args))
		    (if (null? l) 
			'()
			(cons (get-first-val ((car l) rte))
			      (loop (cdr l)))))))
	(fun:apply fun args rte)))))

;; ---------------------------------------------------------------------- ;;
;; Les suites d'expressions (body)                                        ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-body cte body)
  (if (null? body)
      (gen:gen-cst $the-false-value)
      (let ((expr (car body))
	    (rest (cdr body)))
	(cond
	 ((local-decl? expr)
	  (let ((decl (local-decl-declaration expr)))
	    (cond
	     ((ast-let? decl)
	      (gen:gen-let cte decl rest))
	     ((local-method? decl)
	      (gen:gen-local cte decl rest))
	     ((let-handler? decl)
	      (gen:gen-handler cte decl rest))
	     (else
	      (dylan:error " *** Internal error in `gen:gen-body' ***")))))
	 ((defining-form? expr)
	  (dylan:error "Definition must be at top-level"))
	 ((expr? expr)
	  (let ((comp-expr (gen:gen-expr cte (expr-expr expr))))
	    (if (null? rest)
		(lambda (rte)
		  (comp-expr rte))
		(let ((comp-rest (gen:gen-body cte rest)))
		  (lambda (rte)
		    (comp-expr rte)
		    (comp-rest rte))))))
	 (else
	  (dylan:error " *** Internal error in `gen:gen-body' ***"))))))

;; ---------------------------------------------------------------------- ;;
;; Les énoncés IF                                                         ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-if cte test then-body else-body)
  (let ((comp-test (gen:gen-expr cte test))
	(comp-then (gen:gen-body cte then-body))
	(comp-else (if (ast-if? else-body)
		       (gen:gen-if cte 
				   (ast-if-test else-body)
				   (ast-if-then-part else-body)
				   (ast-if-else-part else-body))
		       (gen:gen-body cte else-body))))
    (lambda (rte)
      (if (eq? (get-first-val (comp-test rte)) $the-false-value)
	  (comp-else rte)
	  (comp-then rte)))))

;; ---------------------------------------------------------------------- ;;
;; Les énoncés CASE                                                       ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-case cte clauses)
  (define (gen-or-list l)
    (let ((comp-expr (gen:gen-expr cte (car l))))
      (if (null? (cdr l))
	  (get-first-val comp-expr)
	  (let ((comp-rest (gen-or-list (cdr l))))
	    (lambda (rte)
	      (let ((v (get-first-val (comp-expr rte))))
		(if (eq? v $the-false-value)
		    (comp-rest rte)
		    $the-true-value)))))))
  
  (let loop ((l clauses))
    (if (null? l)
	(gen:gen-cst $the-false-value)
	(let* ((cl (car l)))
	  (cond
	   ((cond-clause? cl)
	    (let* ((matches   (cond-clause-matches cl))
		   (body      (cond-clause-body cl))
		   (comp-test (gen-or-list matches))
		   (comp-body (gen:gen-body cte body))
		   (comp-rest (loop (cdr l))))
	      (lambda (rte)
		(let ((test (comp-test rte)))
		  (if (eq? test $the-false-value)
		      (comp-rest rte)
		      (comp-body rte))))))
	   ((other-clause? cl)
	    (gen:gen-body cte (other-clause-body cl)))
	   (else
	    (dylan:error "Internal error in `gen:gen-case'")))))))


;; ---------------------------------------------------------------------- ;;
;; Les énoncés SELECT ...                                                 ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-select cte target test clauses)

;; --- Les tests ... ---------------------------------------------------- ;;
  (define (gen-or-list l)
    (let ((comp-expr (gen:gen-expr cte (car l))))
      (if (null? (cdr l))
	  (lambda (target test rte) 
	    (get-first-val
	     (fun:apply test (list target (comp-expr rte)) rte)))
	  (let ((comp-rest (gen-or-list (cdr l))))
	    (lambda (target test rte)
	      (let ((v (get-first-val 
			(fun:apply test (list target (comp-expr rte)) rte))))
		(if (eq? v $the-false-value)
		    (comp-rest target test rte)
		    $the-true-value)))))))
  
;; --- Les clauses ... -------------------------------------------------- ;;
  (define (gen-clauses l)
    (cond
     ((null? l)
      (lambda (target test rte) 
	(dylan:error "No match in `select' form")))
     (else
      (let ((cl (car l)))
	(cond
	 ((cond-clause? cl)
	  (let ((comp-rest    (gen-clauses (cdr l)))
		(comp-matches (gen-or-list (cond-clause-matches cl)))
		(comp-body    (gen:gen-body cte (cond-clause-body cl))))
	    (lambda (target test rte)
	      (let ((v (comp-matches target test rte)))
		(if (eq? v $the-false-value)
		    (comp-rest target test rte)
		    (comp-body rte))))))
	 ((other-clause? cl)
	  (let ((comp-body (gen:gen-body cte (other-clause-body cl))))
	    (lambda (target test rte)
	      (comp-body rte))))
	 (else
	  (dylan:error "Internal error in `gen:gen-select'")))))))

  (let ((comp-test    (if test (gen:gen-expr cte test) (gen:gen-cst fun:==)))
	(comp-target  (gen:gen-expr cte target))
	(comp-clauses (gen-clauses clauses)))
    (lambda (rte)
      (let ((test   (comp-test rte))
	    (target (comp-target rte)))
	(comp-clauses target test rte)))))


;; ---------------------------------------------------------------------- ;;
;; Extrait la fonction de test et les arguments d'initialisation          ;;
;; d'une déclaration de `handler'.                                        ;;
;; ---------------------------------------------------------------------- ;;
(define (find-test-and-args cte l)
  (let loop ((l l) (test #f) (initargs #f))
    (cond
     ((null? l)
      (cons test initargs))
     (else
      (let ((key  (keyword-name (caar l)))
	    (val  (cdar l))
    (rest (cdr l)))
	(cond
	 ((eq? key 'TEST)
	  (if test
	      (loop (cdr l) test initargs)
	      (loop (cdr l) (gen:gen-expr cte val) initargs)))
	 ((eq? key 'INIT-ARGUMENTS)
	  (if initargs
	      (loop (cdr l) test initargs)
	      (loop (cdr l) test (gen:gen-expr cte val))))
	 (else
	  (error:signal
	   (class:make 
	    <simple-warning>
	    (make-instance <symbol> 'FORMAT-STRING)
	    (make-instance <byte-string> 
			   "[* WARNING: unhandled keyword %= *]")
	    (make-instance <symbol> 'FORMAT-ARGUMENTS)
	    (make-instance <pair> (cons (make-instance <symbol> key)
					$the-empty-list))))
	  (loop (cdr l) test initargs))))))))

;; ---------------------------------------------------------------------- ;;
;; Les énoncés BLOCK ...                                                  ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-block cte var body epilog)
  (define (gen-excepn-clauses l)
    (cond
     ((null? l)
      (lambda (exit ex-top cl-top rte) $the-false-value))
     (else
      (let* ((cl        (car l))
	     (name      (exception-name cl))
	     (comp-type (gen:gen-expr cte (exception-type cl)))
	     (test/args (find-test-and-args cte (exception-prop-list cl)))
	     (test      (car test/args))
	     (comp-test (if test test (gen:gen-cst $default-test)))
	     (args      (cdr test/args))
	     (comp-args (if args args (gen:gen-cst $the-empty-list)))
	     (new-cte   (cons (list name) cte))
	     (comp-body (gen:gen-body new-cte (exception-body cl)))
	     (comp-rest (gen-excepn-clauses (cdr l))))
	(lambda (exit ex-top cl-top rte)
	  (let ((type (comp-type rte))
		(test (comp-test rte))
		(args (comp-args rte)))
	    (error:establish-handler
	     type
	     test
	     args
	     (fun:make-predef
	      'exc-handler 
	      (list (list name <condition>) (list 'ignore <function>))
	      #f '() #f (make-return-value '() #f #f)
	      (lambda (args)
		(let ((cnd (car args))
		      (next (cadr args)))
		  (error:pop-handlers ex-top)
		  (clean:run-and-pop cl-top)
		  (let* ((new-rte (let ((l (link rte 1)))
				    (add-binding l 1 name type cnd)
				    l))
			 (vals    (comp-body new-rte)))
		    (fun:apply exit (if (pair? vals) vals (list vals)) rte)))))))
		   
	  (comp-rest exit ex-top cl-top rte))))))
    
  (define (make-exit-proc)
    (fun:make-predef '*exit-proc* '() #t #f #f
;; --- A modifier [ ----------------------------------------------------- ;;
		     (make-return-value '() #f #f)
;; --- ] ---------------------------------------------------------------- ;;
		     #f))
  
  (define (bind-proc meth proc)
    (method-code-set! (instance-slots meth) proc))
  
  (let* ((new-cte    (cons (list var) cte))
	 (comp-body  (gen:gen-body new-cte body))
	 (cleanup    (if epilog (block-epilog-cleanup epilog) #f))
	 (comp-clean (if cleanup 
			 (gen:gen-body new-cte cleanup) 
			 (gen:gen-cst $the-false-value)))
	 (comp-excn  (gen-excepn-clauses (block-epilog-excepts epilog)))
	 (exit-proc (make-exit-proc)))
      (lambda (rte)
	(let* ((excepts-top (error:get-top-of-stack))
	       (cleanup-top (clean:get-top-of-stack))
	       (new-rte     (let ((l (link rte 1)))
			      (add-binding l 1 var <compiled-method> exit-proc)
			      l))
	       (execed?     #f)
	       (result      (bind-exit (k)
			       (clean:make-cleanup
				exit-proc
				(lambda () (comp-clean new-rte)))
			       (bind-proc 
				exit-proc 
				(lambda (args) 
				  (clean:run-and-pop cleanup-top)
				  (k args)))
			       (comp-excn 
				exit-proc excepts-top cleanup-top new-rte)
			       (comp-body new-rte))))
	  (clean:run-and-pop cleanup-top)
	  (error:pop-handlers excepts-top)
	  result))))

;	       (result      (call/cc
;			     (lambda (k)
;			       (clean:make-cleanup
;				exit-proc
;				(lambda () (comp-clean new-rte)))
;			       (bind-proc 
;				exit-proc 
;				(lambda (args) (k args)))
;			       (comp-excn 
;				exit-proc excepts-top cleanup-top new-rte)
;			       (comp-body new-rte)))))

    
;; ---------------------------------------------------------------------- ;;
;; Les énoncés WHILE                                                      ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-while cte test body)
  (let ((comp-test (gen:gen-expr cte test))
	(comp-body (gen:gen-body cte body)))
    (lambda (rte)
      (let loop ((t (get-first-val (comp-test rte))))
	(if (not (eq? t $the-false-value))
	    (begin
	      (comp-body rte)
	      (loop (comp-test rte)))
	    $the-false-value)))))

;; ---------------------------------------------------------------------- ;;
;; Les énoncés UNTIL                                                      ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-until cte test body)
  (let ((comp-test (gen:gen-expr cte test))
	(comp-body (gen:gen-body cte body)))
    (lambda (rte)
      (let loop ((t (get-first-val (comp-test rte))))
	(if (eq? t $the-false-value)
	    (begin
	      (comp-body rte)
	      (loop (comp-test rte)))
	    $the-false-value)))))

;; ---------------------------------------------------------------------- ;;
;; Les énoncés FOR ...                                                    ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-for cte header body result-body)

;; --- Précompilation des clauses (évaluations uniques) ----------------- ;;
  (define (precomp-cl aux-cte cl)
    (cond
     ((for-numeric? cl)
      (let* ((var        (for-numeric-var cl))
	     (name       (variable-name var))
	     (get-name   (gen:gen-ref aux-cte name))
	     (type       (variable-type var))
	     (comp-type  (if type 
			     (gen:gen-expr cte type)
			     (gen:gen-cst <number>)))
	     (comp-start (gen:gen-expr cte (for-numeric-start cl)))
	     (bound      (for-numeric-bound cl))
	     (comp-bound (if bound 
			     (let ((boundt (car bound))
				   (comp-expr (gen:gen-expr cte (cdr bound))))
			       (lambda (rte)
				 (cons boundt (comp-expr rte))))
			     (lambda (rte) #f)))
	     (step       (for-numeric-step cl))
	     (comp-step  (if step
			     (gen:gen-expr cte step)
			     (gen:gen-cst (predef:make-int 1)))))
	(lambda (rte)
	  (let ((start (comp-start rte))
		(step  (comp-step rte))
		(bound (comp-bound rte)))
	    (error:check-type start <number>)
	    (error:check-type step  <number>)
	    (if bound (error:check-type (cdr bound) <number>))
	    (list 'numeric name (comp-type rte) start step bound get-name)))))
     
     ((for-explicit? cl)
      (let* ((var        (for-explicit-vars cl))
	     (name       (variable-name var))
	     (type       (variable-type var))
	     (comp-type  (if type 
			     (gen:gen-expr cte type)
			     (gen:gen-cst <object>)))
	     (comp-start (gen:gen-expr cte (for-explicit-init-val cl)))
	     (comp-next  (gen:gen-expr aux-cte (for-explicit-next-val cl))))
	(lambda (rte)
	  (list 'explicit 
		name 
		(comp-type rte) 
		(comp-start rte)
		comp-next))))))

;; --- Précompilatio d'une collection ----------------------------------- ;;
  (define (precomp-collection cl)
    (let* ((var        (for-collection-var cl))
	   (name       (variable-name var))
	   (type       (variable-type var))
	   (comp-type  (if type (gen:gen-expr cte type) (gen:gen-cst <object>)))
	   (comp-coll  (gen:gen-expr cte (for-collection-coll cl))))
      (lambda (rte)
	(let* ((coll (comp-coll rte))
	       (type (comp-type rte))
	       (protoco (fun:apply fwd-protoco-gf (list coll) rte)))
	  (list->vector (append (list name type coll) protoco))))))
  
;; --- Création de l'environnement local -------------------------------- ;;
  (define (init-cl cl-list rte)
    (let loop ((i 1) (l cl-list))
      (if (pair? l)
	  (let* ((cl       (car l))
		 (name     (list-ref cl 1))
		 (type     (list-ref cl 2))
		 (start    (list-ref cl 3)))
	    (add-binding rte i name type start)
	    (loop (+ i 1) (cdr l))))))

;; --- Création des fonctions de mise à jour ---------------------------- ;;
  (define (update-cl rte fs cl-list)
    (let ((new-rte (link (vector-ref rte 0) fs)))
      (let loop ((i 1) (cl-list cl-list))
	(if (null? cl-list)
	    new-rte
	    (let* ((cl       (car cl-list))
		   (t        (car cl))
		   (name     (list-ref cl 1))
		   (type     (list-ref cl 2))
		   (step     (list-ref cl 4))
		   (old-val  (binding-value (vector-ref rte i))))
	      (cond
	       ((eq? t 'numeric)
		(add-binding new-rte i name type
			     (fun:apply +-gf (list old-val step) rte)))
	       ((eq? t 'explicit)
		(add-binding new-rte i name type (step rte))))
	      (loop (+ i 1) (cdr cl-list)))))))

;; --- Ajout des collections à l'environnement local -------------------- ;;
  (define (init-coll rte fs-base colls)
    (let loop ((i fs-base) (colls colls))
      (if (pair? colls)
	  (let* ((collv (car colls))
		 (name  (vector-ref collv 0))
		 (type  (vector-ref collv 1))
		 (coll  (vector-ref collv 2))
		 (limit (vector-ref collv 4))
		 (nextf (vector-ref collv 5))
		 (eltf  (vector-ref collv 8))
		 (state (vector-ref collv 3))
		 (obj   (fun:apply eltf (list coll state) rte))
		 (newst (fun:apply nextf (list coll state) rte)))
	    (vector-set! collv 3 newst)
	    (add-binding rte i name type obj)
	    (loop (+ i 1) (cdr colls))))))
  
;; --- Les tests "d'exhaustion" ... ------------------------------------- ;;
  (define (gen-check-exhaustion cl-list)
    (if (null? cl-list)
	(gen:gen-cst $the-false-value)
	(let* ((cl   (car cl-list))
	       (rest (cdr cl-list))
	       (clt  (car cl)))
	  (if (eq? clt 'numeric)
	      (let ((getter (list-ref cl 6))
		    (x      (list-ref cl 5)))
		(if x
		    (let ((boundv (cdr x))
			  (boundt (car x))
			  (stept  (instance-class (list-ref cl 4)))
			  (step   (instance-slots (list-ref cl 4)))
			  (comp-rest (gen-check-exhaustion rest)))
		      (cond
		       ((eq? boundt 'ABOVE)
			(lambda (rte)
			  (let ((v (getter rte)))
			    (if (eq? (fun:apply <-gf (list boundv v) rte)
				     $the-false-value)
				$the-true-value
				(comp-rest rte)))))
;			    (if (<= (instance-slots v) boundv)
;				$the-true-value
;				(comp-rest rte)))))
		       ((eq? boundt 'BELOW)
			(lambda (rte)
			  (let ((v (getter rte)))
			    (if (eq? (fun:apply <-gf (list v boundv) rte)
				     $the-false-value)
				(comp-rest rte)
				$the-true-value))))
;			    (if (>= (instance-slots v) boundv)
;				$the-true-value
;				(comp-rest rte)))))
		       ((or (and (eq? stept <integer>) (fix:< step 0))
			    (and (eq? stept <double-float>) (flo:< step 0.0)))
			(lambda (rte)
			  (let ((v (getter rte)))
			    (if (eq? (fun:apply <-gf (list v boundv) rte)
				     $the-true-value)
				$the-true-value
				(comp-rest rte)))))
;			    (if (< (instance-slots v) boundv)
;				$the-true-value
;				(comp-rest rte)))))
		       (else 
			(lambda (rte)
			  (let ((v (getter rte)))
			    (if (eq? (fun:apply <-gf (list boundv v) rte)
				     $the-true-value)
				$the-true-value
				(comp-rest rte)))))))
;			    (if (> (instance-slots v) boundv)
;				$the-true-value
;				(comp-rest rte)))))))
		      
		    (gen-check-exhaustion rest)))
	      (gen-check-exhaustion rest)))))

;; --- Tests d'exhaustion sur les collections --------------------------- ;;
  (define (gen-check-coll colls)
    (if (null? colls)
	(gen:gen-cst $the-false-value)
	(let* ((collv (car colls))
	       (rest  (gen-check-coll (cdr colls)))
	       (coll  (vector-ref collv 2))
	       (limit (vector-ref collv 4))
	       (fin?  (vector-ref collv 6)))
	  (lambda (rte)
	    (let* ((state (vector-ref collv 3))
		   (result (get-first-val 
			    (fun:apply fin? (list coll state limit) rte))))
	      (if (eq? result $the-false-value)
		  (rest rte)
		  $the-true-value))))))
  
;; --- Compilation d'un test `until' ou `while' ------------------------- ;;
  (define (gen-end-test aux-cte end-test)
    (let ((condit (for-header-type end-test))
	  (comp-test (gen:gen-expr aux-cte (for-header-test end-test))))
      (if (eq? condit 'FOR-WHILE)
;; --- while ... -------------------------------------------------------- ;;
	  (lambda (rte)
	    (if (eq? (get-first-val (comp-test rte)) $the-false-value)
		$the-true-value
		$the-false-value))
;; --- until ... -------------------------------------------------------- ;;
	  (lambda (rte)
	    (if (eq? (get-first-val (comp-test rte)) $the-false-value)
		$the-false-value
		$the-true-value)))))

;; --- Il reste les clauses de collection ... --------------------------- ;;
  
  (let* ((coll-cls      (filter (lambda (x) (for-collection? x)) header))
	 (end-test      (filter (lambda (x) (for-header? x)) header))
	 (other-cls     (filter (lambda (cl) (or (for-numeric? cl)
						 (for-explicit? cl)))
				header))
	 (coll-names    (map (lambda (x) (variable-name (for-collection-var x)))
			     coll-cls))
	 (other-names   (map (lambda (x) 
			       (variable-name (if (for-numeric? x) 
						  (for-numeric-var x)
						  (for-explicit-vars x))))
			     other-cls))
	 (aux-cte       (cons other-names cte))
	 (new-cte       (cons (append other-names coll-names) cte))
	 (precomp-colls (map precomp-collection coll-cls))
	 (precomp-cls   (map (lambda (x) (precomp-cl new-cte x)) other-cls))
	 (comp-body     (gen:gen-body new-cte body))
	 (comp-rest     (if result-body
			    (gen:gen-body aux-cte result-body)
			    (gen:gen-cst $the-false-value)))
	 (end-test      (filter (lambda (x) (for-header? x)) header))
	 (comp-end-test (if (pair? end-test)
			    (gen-end-test aux-cte (car end-test))
			    (gen:gen-cst $the-false-value)))
	 (fs-coll       (length coll-cls))
	 (fs-other      (length other-cls))
	 (fs-coll-base  (+ fs-other 1))
	 (fs            (+ fs-coll fs-other)))

    (lambda (rte)
      (let* ((clauses         (map (lambda (code) (code rte)) precomp-cls))
	     (coll-clauses    (map (lambda (code) (code rte)) precomp-colls))
	     (comp-exhaust?   (gen-check-exhaustion clauses))
	     (comp-exh-colls? (gen-check-coll coll-clauses))
	     (new-rte         (link rte fs)))
	(init-cl clauses new-rte)
	(let loop ((new-rte new-rte))
	  (if (or (eq? (comp-exhaust? new-rte) $the-true-value)
		  (eq? (comp-exh-colls? new-rte) $the-true-value)
		  (eq? (comp-end-test new-rte) $the-true-value))
	      (comp-rest new-rte)
	      (begin
		(init-coll new-rte fs-coll-base coll-clauses)
		(comp-body new-rte)
		(loop (update-cl new-rte fs clauses)))))))))

;; ---------------------------------------------------------------------- ;;
;; Les énoncés LET                                                        ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-let cte decl body)
  (let* ((decls      (ast-let-names decl))
	 (rest?      (rest-param? (car (reverse decls))))
	 (framesize  (length decls))
	 (names      (map (lambda (v) 
			    (if (variable? v) 
				(variable-name v)
				(rest-param-name v)))
			  decls))
	 (new-cte    (cons names cte))
	 (comp-types (map (lambda (v) 
			    (if (variable? v)
				(let ((t (variable-type v)))
				  (if t
				      (gen:gen-expr cte t)
				      (gen:gen-cst <object>)))
				(gen:gen-cst <sequence>)))
			  decls))
	 (values     (gen:gen-expr cte (ast-let-value decl)))
	 (comp-body  (gen:gen-body new-cte body)))
    (lambda (rte)
      (let ((vals    (values rte))
	    (new-rte (link rte framesize)))
	(let loop ((names names)
		   (types comp-types)
		   (l     (if (lst? vals) vals (list vals)))
		   (i     1))
	  (add-binding new-rte i (car names) ((car types) rte) $unbound)
	  (let* ((p? (pair? l))
		 (val (if p? (car l) $the-false-value))
		 (rst (if p? (cdr l) '()))
		 (bnd (vector-ref new-rte i)))
	    (if (= i framesize)
		(env:set! (if rest? (predef:make-list l) val) bnd)
		(begin
		  (env:set! val bnd)
		  (loop (cdr names) (cdr types) rst (+ i 1))))))
	(comp-body new-rte)))))



;; ---------------------------------------------------------------------- ;;
;; Les énoncés LOCAL                                                      ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-local cte decls body)

  (define (comp-method-decl cte decls)
    (if (null? decls)
	'()
	(let* ((meth (car decls))
	       (name (named-method-name meth)))
	  (cons (gen:gen-method 
		 cte
		 name
		 #f
		 (named-method-params meth)
		 (named-method-return-type meth)
		 (named-method-body meth))
		(comp-method-decl cte (cdr decls))))))
  
  (let* ((meths       (local-method-list decls))
	 (names       (map (lambda (x) (named-method-name x)) meths))
	 (new-cte     (cons names cte))
	 (framesize   (length names))
	 (comp-meths  (comp-method-decl new-cte meths))
	 (comp-body   (gen:gen-body new-cte body)))
    (lambda (rte)
      (let ((new-rte (link rte framesize)))
	(let loop ((names names)
		   (meths comp-meths)
		   (i 1))
	  (if (pair? names)
	      (begin
		(add-binding 
		 new-rte i (car names) <user-method> ((car meths) new-rte))
		(loop (cdr names) (cdr meths) (+ i 1)))))
	(comp-body new-rte)))))

;; ---------------------------------------------------------------------- ;;
;; Les formes LET HANDLER ...                                             ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-handler cte decl rest)
  (let* ((comp-rest (gen:gen-body cte rest))
	 (type      (let-handler-type decl))
	 (handler   (let-handler-expr decl))
	 (test/args (find-test-and-args cte (let-handler-prop-list decl)))
	 (test      (car test/args))
	 (comp-test (if test test (gen:gen-cst $default-test)))
	 (args      (cdr test/args))
	 (comp-args (if args args (gen:gen-cst $the-empty-list)))
	 (comp-type (gen:gen-expr cte type))
	 (comp-hand (gen:gen-expr cte handler)))
    (lambda (rte)
      (let* ((type (comp-type rte))
	     (test (comp-test rte))
	     (args (comp-args rte))
	     (hdlr (comp-hand rte))
	     (top  (error:get-top-of-stack)))
	(error:establish-handler type test args hdlr)
	(let ((val (comp-rest rte)))
	  (error:pop-handlers top)
	val)))))


;; ---------------------------------------------------------------------- ;;
;; Les methodes (nommes et anonymes) ...                                  ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:gen-method cte name next-meth param-list values body)
  
  (define arg-names '())
  (define next-name (if next-meth (list next-meth) '()))
  (define rest-name '())
  (define key-names '())
  
  ;; parse arguments
  (let loop ((l param-list) 
	     (fs (if next-meth 1 0))
	     (pos '()) 
	     (next next-meth) 
	     (rest #f) 
	     (keys '()) 
	     (all-keys #f))
    (if (pair? l)
	(let ((param (car l)))
	  (cond

	   ((position-param? param)
	    (let ((name (position-param-name param))
		  (def-type (if (position-param-type param)
				(gen:gen-expr cte (position-param-type param))
				(if (position-param-singleton param)
				    (let ((code (gen:gen-expr cte 
						 (position-param-singleton param))))
				      (lambda (rte) 
					(make-instance <singleton> (code rte))))
				    (gen:gen-cst <object>)))))
	      (set! arg-names (append arg-names (list name)))
	      (loop (cdr l) (+ fs 1)
		    (cons (list name def-type) pos)
		    next rest keys all-keys)))

	   ((next-param? param)
	    (let ((name (next-param-name param)))
	      (set! next-name (list name))
	      (loop (cdr l) 
		    (if next fs (+ fs 1))
		    pos name rest keys all-keys)))
	     
	   ((rest-param? param)
	    (let ((name (rest-param-name param)))
	      (set! rest-name (list name))
	      (loop (cdr l) (+ fs 1) pos next name keys all-keys)))
	     
	   ((keyword-param? param)
	    (let* ((new-cte  (cons (append arg-names next-name rest-name key-names) 
				   cte))
		   (key-var  (keyword-param-variable param))
		   (key-name (let ((name (keyword-param-keyword param)))
			       (if name (keyword-name name) key-var)))
		   (default  (let ((expr (keyword-param-default param)))
			       (if expr 
				   (gen:gen-expr new-cte expr) 
				   (gen:gen-cst $the-false-value)))))
	      (set! key-names (append key-names (list key-var)))
	      (loop (cdr l) (+ fs 1) pos next rest 
		    (cons (list key-name key-var default) keys) all-keys)))
	     
	   ((all-keys? param)
	    (loop (cdr l) fs pos next rest keys #t))))
	     
  
	(let* ((new-cte       (cons (append arg-names next-name rest-name key-names) 
				    cte))
	       (comp-body     (gen:gen-body new-cte body))
	       (nreq          (length pos))
	       (pos           (reverse pos))
	       (keys          (if keys (reverse keys) keys)))
	  (lambda (rte)
	    (let ((spec (gen:eval-spec pos rte)))
	      (make-instance 
	       <user-method>
	       (make-method name fs nreq spec rest keys all-keys #f ; vals
			    next rte comp-body))))))))


;; ---------------------------------------------------------------------- ;;
;; L'evaluation des "specialiseurs" ...                                   ;;
;; ---------------------------------------------------------------------- ;;
(define (gen:eval-spec l rte)
  (let loop ((l l))
    (if (null? l)
	'()
	(let ((name (caar l))
	      (type ((cadar l) rte)))
	  (error:check-type type <type>)
	  (cons (list name type) (loop (cdr l)))))))

;; --- Fin du fichier `gen.scm' ----------------------------------------- ;;
