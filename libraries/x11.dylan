// ---------------------------------------------------------------------- //
// FICHIER               : x11.dylan                                      //
// DATE DE CREATION      : Mon May 29 10:23:34 1995                       //
// DERNIERE MODIFICATION : Fri Jun  9 10:04:48 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// X initialization routines                                              //
// ---------------------------------------------------------------------- //

define open class <window> (<object>)
  slot name :: <byte-string>, required-init-keyword: name: ;
  slot width :: <integer>, init-keyword: width:, init-value: 100 ;
  slot height :: <integer>, init-keyword: height:, init-value: 100 ;
  slot id :: <integer> ;
end ;

define method initialize (w :: <window>, #all-keys)
  w.id := x11-make-window (w.name, w.width, w.height) ;
end ;

define method draw-line (w :: <window>, color :: <integer>,
			 x1 :: <integer>, y1 :: <integer>,
			 x2 :: <integer>, y2 :: <integer>)
  x11-draw-line (w.id, color, x1, y1, x2, y2) ;
end ;

define method close (w :: <window>)
  if (~ (w.id == -1))
    w.id := x11-close-window (w.id) ;
  end ;
end ;
