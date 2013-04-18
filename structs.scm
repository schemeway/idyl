;; ---------------------------------------------------------------------- ;;
;; FICHIER               : structs.scm                                    ;;
;; DATE DE CREATION      : Mon May 29 09:57:03 1995                       ;;
;; DERNIERE MODIFICATION : Mon May 29 09:57:29 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Structures declarations ...                                            ;;
;; ---------------------------------------------------------------------- ;;

;; ---------------------------------------------------------------------- ;;
;; Interface de l'AST                                                     ;;
;; ---------------------------------------------------------------------- ;;

;; --- Les déclarations ... --------------------------------------------- ;;
(def-struct defining-form  type)
(def-struct local-decl     declaration)
(def-struct expr           expr)
(def-struct def-class      flags name super guts)
(def-struct def-constant   names value)
(def-struct def-generic    flags name params return-type prop-list)
(def-struct def-method     flags name params return-type body)
(def-struct def-variable   names value)
(def-struct ast-let        names value)
(def-struct let-handler    type prop-list expr)
(def-struct local-method   list)
(def-struct variable       name type)
(def-struct named-method   name params return-type body)
(def-struct keyword        name)

;; --- Les expressions ... ---------------------------------------------- ;;
;; ------- to be modified 
(def-struct binop-series   list)
;; -------

(def-struct binary-op      name)
(def-struct unary-op       operator operand)
(def-struct ast-symbol     name)
(def-struct array-ref      seq element)
(def-struct funcall        function arguments)
(def-struct anonym-method  descr)
(def-struct slot-reference object slot)
(def-struct assignment     place new-value)
(def-struct or-expr        form1 form2)
(def-struct and-expr       form1 form2)
(def-struct statement      statement)
(def-struct true-constant  )
(def-struct false-constant )
(def-struct concat-string  list)
(def-struct character      value)
(def-struct ast-integer    value)
(def-struct float          value)
(def-struct literal-symbol name)
(def-struct dotted-list    before after)
(def-struct literal-list   elements)
(def-struct literal-vector elements)

;; --- Les énoncés ... -------------------------------------------------- ;;
(def-struct begin-form     body)
(def-struct block-exit     symbol body epilog)
(def-struct block-epilog   cleanup excepts)
(def-struct ast-case       cond-body)
(def-struct ast-if         test then-part else-part)
(def-struct ast-for        header body final-part)
(def-struct for-header     type test)
(def-struct for-explicit   multi? vars init-val next-val)
(def-struct for-collection var key coll)
(def-struct for-numeric    var start bound step)
(def-struct select         target-form test cases)
(def-struct until          test body)
(def-struct while          test body)
(def-struct exception      name type prop-list body)
(def-struct cond-clause    matches body)
(def-struct other-clause   body)

;; --- Les classes ... -------------------------------------------------- ;;
(def-struct slot-spec      flags alloc name type prop-list)
(def-struct keyword-spec   required? name prop-list)
(def-struct inherited-spec name prop-list)

;; --- Les fonctions génériques ... ------------------------------------- ;;
(def-struct return-type    head rest)
(def-struct position-param name type singleton)
(def-struct next-param     name)
(def-struct rest-param     name)
(def-struct all-keys       )
(def-struct gf-keyword     keyword symbol type)
(def-struct keyword-param  keyword variable type default)

;; ---------------------------------------------------------------------- ;;
;; Les déclarations de modules ...                                        ;;
;; ---------------------------------------------------------------------- ;;
(def-struct module-defn   name clauses)
(def-struct export-clause names)
(def-struct create-clause names)
(def-struct use-clause    name options)
(def-struct prefix-opt    string)
(def-struct import-opt    imports)
(def-struct exclude-opt   nameset)
(def-struct export-opt    nameset)
(def-struct rename-opt    specs)

;; ---------------------------------------------------------------------- ;;
;; La représentation d'un module.                                         ;;
;; ---------------------------------------------------------------------- ;;
(def-struct module
  name
  exports				; les variables exportées
  creates				; les variables créées
  rename-alist				; les variables renommées
  env)


;; ---------------------------------------------------------------------- ;;
;; La représentation des classes et instances ...                         ;;
;; ---------------------------------------------------------------------- ;;
(def-struct class 
  name					; nom de la classe (symbole Scheme)
  instantiated?				; La classe a-t-elle été instantiée?
  sealed? abstract? primary?		; qualificatifs de la classe
  supers				; super-classes
  direct-subs				; sous-classes directes
  prec-list				; liste de précédence
  slots					; ==> liste d'association (nom . slot-struct)
  class-slots				; ==> slots de la classe (allocation = class)
  init-args				; ==> liste d'association (cle . ...)
  )

(def-struct slot
  class-def				; classe où la slot a été déclarée
  getter-name				; nom du `getter'
  setter-name				; nom du `setter'
  type deferred-type			; type et type différé
  init-value init-function		; valeur et fonction initiales
  init-key req-init-key
  allocation)				; qualificatif d'allocation

(def-struct init-arg
  required? 
  type
  init-value
  init-function)

(def-struct singleton
  object)

(def-struct instance
  class slots)


;; ---------------------------------------------------------------------- ;;
;; Les méthodes et fonctions génériques ...                               ;;
;; ---------------------------------------------------------------------- ;;
(def-struct generic
  name nreq spec rest? keys all-keys? values methods code)

(def-struct method
  name fs nreq spec rest? keys all-keys? values next-name env code)

(def-struct return-value
  req rest? type)


;; ---------------------------------------------------------------------- ;;
;; Les environnements ...                                                 ;;
;; ---------------------------------------------------------------------- ;;
(def-struct binding 
  name value type constant?)

;; --- Fin du fichier `structs.scm' ------------------------------------- ;;
