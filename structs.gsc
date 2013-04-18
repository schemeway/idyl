;; ---------------------------------------------------------------------- ;;
;; FICHIER               : structs.gsc                                    ;;
;; DATE DE CREATION      : Mon May 29 09:56:14 1995                       ;;
;; DERNIERE MODIFICATION : Mon May 29 09:56:23 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; Gambit-C specific macros ...                                           ;;
;; ---------------------------------------------------------------------- ;;


(##define-macro (def-struct name . field-defs)

  (define (err)
    (##signal '##SIGNAL.SYNTAX-ERROR
              (##cons 'define-structure (##cons name field-defs))
              "Ill-formed special form:"
              'define-structure))

  (define (sym . strings)
    (##string->symbol (##apply ##string-append strings)))

  (if (##symbol? name)
    (let* ((name-str (##symbol->string name))
           (tag (sym "##structure-tag." name-str)))

      (define (add-field i field fields)
        (let* ((field-str (##symbol->string field))
               (field-ref (sym name-str "-" field-str))
               (field-set! (sym name-str "-" field-str "-set!")))
          (##cons `(##DEFINE-MACRO (,field-set! X Y)
		      (##LIST '##VECTOR-SET! X ,i Y))
                  (##cons `(##DEFINE-MACRO (,field-ref X)
                             (##LIST '##VECTOR-REF X ,i))
                          fields))))

      (define (generate-definitions field-ops all-fields printed-fields)
        (let ((params (##reverse all-fields)))
          `(BEGIN
             (SET! ,tag '#(,name ,@(##reverse printed-fields)))
             ',name)))

      (let loop1 ((l1 field-defs) (l2 '()) (l3 '()) (i 1))
        (if (##pair? l1)
          (let ((rest (##cdr l1)) (field (##car l1)))
            (cond ((##symbol? field)
                   (loop1 rest
                         (add-field i field l2)
                         (##cons field l3)
                         (##fixnum.+ i 1)))
                  ((and (or (##null? field) (##pair? field))
                        (##not (##pair? rest)))
                   (let ((printed-fields l3))
                     (let loop2 ((l1 field) (l2 l2) (l3 l3) (i i))
                       (if (##pair? l1)
                         (let ((rest (##cdr l1)) (field (##car l1)))
                           (cond ((##symbol? field)
                                  (loop2 rest
                                        (add-field i field l2)
                                        (##cons field l3)
                                        (##fixnum.+ i 1)))
                                  (else
                                   (err))))
                         (generate-definitions l2 l3 printed-fields)))))
                  (else
                   (err))))
          (generate-definitions l2 l3 l3))))
    (err)))
