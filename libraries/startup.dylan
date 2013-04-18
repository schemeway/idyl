// ---------------------------------------------------------------------- //
// FICHIER               : startup.dylan                                  //
// DATE DE CREATION      : Mon May 29 10:09:31 1995                       //
// DERNIERE MODIFICATION : Thu Jun  1 14:16:37 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// IDyl startup file ...                                                  //
// ---------------------------------------------------------------------- //

// ---------------------------------------------------------------------- //
// Comparisons (Chapter 5)                                                //
// ---------------------------------------------------------------------- //
define method \= (o1 :: <object>, o2 :: <object>)
  o1 == o2
end ;

define constant \~= =
  method (o1 :: <object>, o2 :: <object>)
    ~ (o1 = o2)
  end ;

define constant \> =
  method (o1 :: <object>, o2 :: <object>)
    (o2 < o1)
  end ;

define constant \<= =
  method (o1 :: <object>, o2 :: <object>)
    ~ (o2 < o1) 
  end ;

define constant \>= =
  method (o1 :: <object>, o2 :: <object>)
    ~ (o1 < o2) 
  end ;

// ---------------------------------------------------------------------- //
// Characters and Symbols .. (chapter 10)                                 //
// ---------------------------------------------------------------------- //
define method as (c == <character>, i :: <integer>)
  if (-1 < i & i < 256)
    int$char (i)
  else
    error ("index out of range: as (<character>, ...)")
  end ;
end ;

define method as (cl == <integer>, c :: <character>)   char$int (c)      end ;
define method as (cl == <symbol>,  s :: <byte-string>) string$symbol (s) end ;
define method as (cl == <string>,  s :: <symbol>)      symbol$string (s) end ;
define method as (cl == <string>,  s :: <number>)      num$string (s)    end ;


// ---------------------------------------------------------------------- //
// Other libraries ...                                                    //
// ---------------------------------------------------------------------- //
load ("LIB_DIR/error.dylan") ;
load ("LIB_DIR/coll.dylan") ;
load ("LIB_DIR/fun.dylan") ;
load ("LIB_DIR/numbers.dylan") ;
load ("LIB_DIR/x11.dylan") ;





