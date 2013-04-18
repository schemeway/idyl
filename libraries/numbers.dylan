// ---------------------------------------------------------------------- //
// FICHIER               : numbers.dylan                                  //
// DATE DE CREATION      : Mon May 29 10:08:12 1995                       //
// DERNIERE MODIFICATION : Mon May 29 10:09:08 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// General Arithmetic Functions ...                                       //
// ---------------------------------------------------------------------- //

  
define method odd? (n :: <integer>)
  if (modulo (n, 2) = 1) #t else #f end ;
end ;

define method even? (n :: <integer>)
  if (modulo (n, 2) = 0) #t else #f end ;
end ;

define method zero? (n :: <number>)
  if (n = 0) #t else #f end ;
end ;

define method positive? (n :: <number>)
  if (0 < n) #t else #f end ;
end ;

define method negative? (n :: <number>)
  if (n < 0) #t else #f end ;
end ;

define method integral? (n :: <number>)
  if (instance? (n, <integer>)) #t else #f end ;
end ;

// floor is defined internally

define method ceiling (n :: <number>)
  let (fl, rst) = floor (n) ;
  if (rst == 0.0)
    values (fl, rst)
  else
    values (fl + 1, rst - 1.0)
  end ;
end ;

define method round (n :: <number>)
  let (fl, rst) = floor (n) ;
  if (0.5 < rst)
    values (fl + 1, rst - 1.0)
  else
    if (rst == 0.5)
      if (even? (fl))
	values (fl, rst)
      else
	values (fl + 1, rst - 1.0)
      end
    else
      values (fl, rst)
    end
  end ;
end ;

define method truncate (n :: <number>)
  let (fl, rst) = floor (n) ;
  if (abs (n) < abs (fl))
    values (fl + 1, rst - 1.0)
  else
    values (fl, rst)
  end ;
end ;

define method floor/    (x :: <real>, y :: <real>)
  let (fl, rst) = floor (x / y) ;
  values (fl, y * rst) 
end ;

define method ceiling/  (x :: <real>, y :: <real>)
  let (fl, rst) = ceiling (x / y) ;
  values (fl, y * rst)
end ;

define method round/    (x :: <real>, y :: <real>)
  let (fl, rst) = round (x / y) ;
  values (fl, y * rst)
end ;

define method truncate/ (x :: <real>, y :: <real>)
  let (fl, rst) = truncate (x / y) ;
  values (fl, y * rst)
end ;

define method remainder (x :: <real>, y :: <real>)
  let (tr, rst) = truncate/ (x, y) ;
  rst
end ;

define method abs (n :: <number>)
  if (n < 0) negative (n) else n end ;
end ;

define method min (r :: <real>, #rest l)
  let m :: <real> = r ;
  for (x in l)
    if (x < m)
      m := x
    end ;
  finally
    m
  end ;
end ;

define method max (r :: <real>, #rest l)
  let m :: <real> = r ;
  for (x in l)
    if (m < x)
      m := x
    end ;
  finally
    m
  end ;
end ;

