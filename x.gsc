;; ---------------------------------------------------------------------- ;;
;; FICHIER               : x.gsc                                          ;;
;; DATE DE CREATION      : Mon May 29 10:03:06 1995                       ;;
;; DERNIERE MODIFICATION : Mon May 29 10:03:51 1995                       ;;
;; ---------------------------------------------------------------------- ;;
;; Copyright (c) 1995 Dominique Boucher                                   ;;
;; ---------------------------------------------------------------------- ;;
;; C interface for X library (with Gambit-C)                              ;;
;; ---------------------------------------------------------------------- ;;

(define x-open-window 
  (c-lambda (char-string int int) int  "x_open_window"))
(define x-close-window
  (c-lambda (int) void "x_close_window"))
(define x-draw-line
  (c-lambda (int int int int int int) void "x_draw_line"))

;; --- Fin des declarations externes ------------------------------------ ;;


