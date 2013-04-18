// ---------------------------------------------------------------------- //
// FICHIER               : coll.dylan                                     //
// DATE DE CREATION      : Mon May 29 10:06:44 1995                       //
// DERNIERE MODIFICATION : Thu Jun  8 16:20:06 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// Collections library functions ...                                      //
// ---------------------------------------------------------------------- //

define open abstract class <explicit-key-collection> (<collection>) end ;
define open abstract class <stretchy-collection>     (<collection>) end ;
define open abstract class <mutable-explicit-key-collection>
    (<explicit-key-collection>, <mutable-collection>) end ;
define open class <table>
    (<mutable-explicit-key-collection>, <stretchy-collection>) end ;
define open class <stretchy-vector> (<stretchy-collection>, <vector>) end ;
define open class <deque> (<stretchy-collection>, <mutable-sequence>) end ;

define open class <range> (<sequence>)
  slot first-val :: <number>, init-keyword: from: ;
  slot step-val  :: <number>, init-keyword: by: ;
  slot last-val  :: <number>;
  keyword from:,  type: <number>, init-value: 0 ;
  keyword by:,    type: <number>, init-value: 1 ;
  keyword to:,    type: <object>, init-value: #f ;
  keyword above:, type: <object>, init-value: #f ;
  keyword below:, type: <object>, init-value: #f ;
  keyword size:,  type: <object>, init-value: #f ;
end ;
  
// ---------------------------------------------------------------------- //
// Les listes ...                                                         //
// ---------------------------------------------------------------------- //
define method make (c == <list>, #key fill (#f), size (0), #all-keys)
  if (size < 0)
    error ("Cannot make a <list> of negative size!") ;
  end ;
  for (s :: <integer> from size to 1 by -1, p = #() then pair (fill, p))
  finally p ;
  end ;
end ;

define method make (self == <empty-list>, #all-keys) #() end ;
define method make (self == <pair>, #key head: h (#f), tail: t (#f), #all-keys)
  pair (h, t) ;
end;

define method element (c :: <list>, i :: <integer>)
  if (i < 0)
    error ("index out of range") ;
  end ;
  for (p = c then tail (p),
       idx = i then idx - 1,
       until (p == #()) | (idx == 0))
  finally
    if (idx == 0 & instance? (p, <pair>))
      head (p)
    else
      error ("index out of range") ;
    end ;
  end ;
end ;

define method empty? (l :: <list>) l == #() end ;

define method add! (l :: <list>, o :: <object>)
  pair (o, l) ;
end ;

define method size (l :: <list>)
  block (return)
    let (l1, l2, len) = values (l, l, 0) ;
    let cont?         =  instance? (l1, <pair>) ;
    while (cont?)
      l1 := tail (l1) ;
      if (l1 == l2)
	 return (#f) ;
      elseif (instance? (l1, <pair>))
	l1 := tail (l1);
	l2 := tail (l2) ;
	len := len + 1 ;
      end ;
      cont? := instance? (l1, <pair>) ;
    end ;
    while (instance? (l2, <pair>))
      len := len + 1 ;
      l2 := tail (l2) ;
    end ;
    return (len) ;
  end ;
end ;

define method forward-iteration-protocol (s :: <list>)
  let sz = size (s) ;
  values (pair (0, s),
	  sz,
	  method (s :: <list>, st :: <pair>)
	    pair (head (st) + 1, tail (tail (st)))
	  end,
	  method (s :: <list>, st :: <pair>, limit :: <integer>)
	    head (st) == limit ;
	  end,
	  method (s :: <list>, st :: <pair>) head (st) end,
	  method (s :: <list>, st :: <pair>) head (tail (st)) end ,
	  method (v :: <object>, s :: <list>, st :: <pair>)
	    head-setter (v, tail (st))
	  end,
	  method (s :: <list>, st :: <pair>)
	    pair (head (st), tail (st))
	  end)
end ;

define method \= (s1 :: <list>, s2 :: <list>)
  block (return)
    let (sp1, sp2) = values (s1, s2) ;
    while (#t)
      if (sp1 == #())
	if (sp2 == #())
	  return (#t)
	else
	  return (#f)
	end
      else
	if (sp2 == #())
	  return (#f)
	else
	  if (instance? (sp1, <pair>) & instance? (sp2, <pair>))
	    if (~ (head (sp1) = head (sp2)))
	      return (#f)
	    else
	      sp1 := tail (sp1) ;
	      sp2 := tail (sp2) ;
	    end
	  else
	    if (sp1 = sp2)
	      return (#t)
	    else
	      return (#f)
	    end if
	  end if
	end if
      end if
    end while
  end block 
end ;


// ---------------------------------------------------------------------- //
// Les matrices (arrays) ...                                              //
// ---------------------------------------------------------------------- //
define method make (c == <array>, #key fill (#f), dimensions: dims (#f), #all-keys)
  let error-str = "illegal `dimensions:' in call to `make (<array>, ...)'" ;
  if (dims)
    if (instance? (dims, <pair>))
      for (x in dims)
	if (~ (instance? (x, <integer>) & x > 0))
	  error (str) ;
	end ;
      end ;
      make$array (dims, fill) ;
    else
      error (error-str) ;
    end ;
  else
    error ("keyword `dimensions:' is required") ;
  end ;
end ;

define method dimensions (a :: <array>) array$dims (a)  end ;

define method size (c :: <array>)
  for (d in dimensions (c), sz = 1 then sz * d)
  finally sz ;
  end
end ;

define method rank (a :: <array>) size (dimensions (a)) end ;

define method element (a :: <array>, idx :: <integer>)
  if ((idx < 0) | ~(idx < size (a)))
    error ("index out of range")
  else
    array$elt (a, idx) ;
  end ;
end ;

define method element-setter (v :: <object>, a :: <array>, idx :: <integer>)
   if ((idx < 0) | ~(idx < size (a)))
    error ("index out of range")
  else
    array$elt-setter (v, a, idx) ;
  end ;
end ; 

define method row-major-index (a :: <array>, #rest indices)
 let a-dims = dimensions (a) ;
 if (size (a-dims) == size (indices))
   let idx = 0;
   for (dp = a-dims then tail (dp),
	i in indices)
     let d    = head (dp);
     let disp = if (tail (dp) == #()) 1 else head (tail (dp)) end ;
     if ((i < 0) | ~(i < d))
       error ("subscript out of range") ;
     else
       idx := (idx + i) * disp ;
     end ;
   finally
     idx
   end ;
 else
   error ("wrong number of subscripts") ;
 end ;
end ;

define method aref (a :: <array>, #rest indices)
  array$elt (a, simple-apply (row-major-index, pair (a, indices)))
end ;

define method aref-setter (v :: <object>, a :: <array>, #rest indices)
  array$elt-setter(v, a, simple-apply (row-major-index, pair (a, indices))) ;
end ;
   
define method dimension (a :: <array>, axis :: <integer>)
  if (axis < 0)
    error ("axis out of range")
  else
    if (axis < rank (a))
      element (dimensions (a), axis)
    else
      error ("axis out of range")
    end ;
  end ;
end ;

// ---------------------------------------------------------------------- //
// Les vecteurs ...                                                       //
// ---------------------------------------------------------------------- //
define method make (c == <vector>, #key fill (#f), size: len (0), #all-keys)
  make (<simple-object-vector>, fill: fill, size: len) ;
end ;

define method make (c == <simple-object-vector>,
		    #key fill (#f), size: len (0),
		    #all-keys)
  if (instance? (len, <integer>))
    if (len < 0)
      error ("vector size cannot be negative")
    else
      make$vector (len, fill)
    end
  else
    error ("vector size should be an integer")
  end
end ;

define constant vector =
  method (#rest args)
    let v = make (<vector>, size: size (args)) ;
    for (x in args, i from 0)
      v [i] := x ;
    end ;
    v ;
  end ;

define method forward-iteration-protocol (s :: <array>)
  let sz = size (s) ;
  values (0,
	  sz,
	  method (s :: <array>, st :: <integer>)
	    st + 1
	  end,
	  method (s :: <array>, st :: <integer>, limit :: <integer>)
	    st == limit
	  end,
	  method (s :: <array>, st :: <integer>) st end,
	  method (s :: <array>, st :: <integer>) s[st] end ,
	  method (v :: <object>, s :: <array>, st :: <integer>)
	    s[st] := v
	  end,
	  method (s :: <array>, st :: <integer>)
	    st
	  end)
end ;

// ---------------------------------------------------------------------- //
// Les chaînes de caractères ...                                          //
// ---------------------------------------------------------------------- //
define method make (c == <byte-string>,
		    #key fill (' '), size: len (0),
		    #all-keys)
  if (len < 0)
    error ("string length cannot be negative")
  else
    if (instance? (fill, <character>))
      make$string (len, fill)
    else
      error ("string fill must be a <character>")
    end ;
  end ;
end ;

define method make (c == <string>, #key fill (' '), size: len (0), #all-keys)
  make (<byte-string>, fill: fill, size: len) ;
end ;

define method dimensions (s :: <byte-string>)
  list (string$size (s)) ;
end;

define method size (s :: <byte-string>)
  string$size (s) ;
end ;

define method element (s :: <byte-string>, i :: <integer>)
  if ((i < 0) | ~(i < size (s)))
    error ("index out of range")
  else
    string$elt (s, i) ;
  end ;
end ;

define method element-setter (v :: <object>, s :: <byte-string>, idx :: <integer>)
   if ((idx < 0) | ~(idx < size (s)))
    error ("index out of range")
  else
     string$elt-setter (v, s, idx) ;
  end ;
end ; 

// ---------------------------------------------------------------------- //
// Les conversions ...                                                    //
// ---------------------------------------------------------------------- //
define method as (c == <vector>, l :: <list>)
  let sz = size (l) ;
  let v  = make (<vector>, size: sz, fill: #f) ;

  for (x in l, i from 0)
    v [i] := x ;
  finally
    v
  end ;
end ;

define method as (c == <list>, v :: <vector>)
  let sz = size (v) ;
  let l  = make (<list>, size: sz, fill: #f) ;
  for (x in v, i from 0, p = l then tail (p))
    head (p) := x ;
  finally
    l
  end ;
end ;