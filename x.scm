;; ---------------------------------------------------------------------- ;;
;; FICHIER               : x.scm                                          ;;
;; DATE DE CREATION      : Mon May 29 10:04:08 1995                       ;;
;; DERNIERE MODIFICATION : Fri Jun  2 11:36:52 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; IDyl's X11 interface ...                                               ;;
;; ---------------------------------------------------------------------- ;;

(define (x:make-window)
  (fun:make-predef
   'x11-make-window
   (list (list 'name <byte-string>) (list 'width <integer>) (list 'height <integer>))
   #f 
  '()
   #f
   (make-return-value (list (list 'result <integer>)) #f #f)
   (lambda (args)
     (let* ((sym (car args))
	    (w   (cadr args))
	    (h   (caddr args))
	    (str (instance-slots sym))
	    (width (instance-slots w))
	    (height (instance-slots h))
	    (win (x-open-window str width height)))
       (predef:make-int win)))))

(define (x:close-window)
  (fun:make-predef
   'x11-close-window
   (list (list 'id <integer>))
   #f 
   '()
   #f
   (make-return-value (list (list 'result <object>)) #f #f)
   (lambda (args)
     (let* ((id-num (car args))
	    (id (instance-slots id-num)))
       (if (= id -1)
	   (dylan:warning "Window already closed")
	   (x-close-window id))
       $the-false-value))))

(define (x:draw-line)
  (fun:make-predef
   'x11-draw-line
   (list (list 'id <integer>) (list 'col <integer>) 
	 (list 'x1 <integer>) (list 'y1 <integer>)
	 (list 'x2 <integer>) (list 'y2 <integer>))
   #f
   '()
   #f
   (make-return-value (list (list 'result <object>)) #f #f)
   (lambda (args)
     (let* ((id-num (list-ref args 0))
	    (c      (list-ref args 1))
	    (x1     (list-ref args 2))
	    (y1     (list-ref args 3))
	    (x2     (list-ref args 4))
	    (y2     (list-ref args 5))
	    (id (instance-slots id-num)))
       (if (= -1 id)
	   (dylan:warning "Cannot draw on closed window")
	   (x-draw-line 
	    id
	    (instance-slots c)
	    (instance-slots x1)
	    (instance-slots y1)
	    (instance-slots x2)
	    (instance-slots y2))))
     $the-false-value)))

(define (x:init)
  (env:add-global-env!
   (env:make-binding 'x11-make-window  <compiled-method> (x:make-window)  #t))
  (env:add-global-env!
   (env:make-binding 'x11-close-window <compiled-method> (x:close-window) #t))
  (env:add-global-env!
   (env:make-binding 'x11-draw-line    <compiled-method> (x:draw-line)    #t)))
