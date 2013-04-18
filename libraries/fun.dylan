// ---------------------------------------------------------------------- //
// FICHIER               : fun.dylan                                      //
// DATE DE CREATION      : Mon May 29 10:07:45 1995                       //
// DERNIERE MODIFICATION : Mon May 29 10:07:53 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// Some functional operators ...                                          //
// ---------------------------------------------------------------------- //

define constant compose =
  method (fun :: <function>, #rest l)
    local method compose-two (f1 :: <function>, f2 :: <function>)
	    method (obj :: <object>) f1 (f2 (obj)) end
	  end ;
    for (f2 in l, f1 = fun then compose-two (f1, f2))
    finally f1 ;
    end ;
  end ;

define constant complement =
  method (pred :: <function>)
    method (#rest l)
      ~ simple-apply (pred, l)
    end
  end ;

define constant always   = method (o :: <object>) method (#rest l) o end end ;
define constant identity = method (o :: <object>) o end ;

define constant apply =
  method (f :: <function>, #rest l)
    if (l == #())
      error ("[* Error: Wrong number of arguments to APPLY *]")
    else
      simple-apply (f, head (l)) ;
    end
  end ;

