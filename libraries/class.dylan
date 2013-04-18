// ---------------------------------------------------------------------- //
// FICHIER               : class.dylan                                    //
// DATE DE CREATION      : Mon May 29 10:06:21 1995                       //
// DERNIERE MODIFICATION : Mon May 29 10:06:29 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// Class-related functions ...                                            //
// ---------------------------------------------------------------------- //

define method make (cl == <singleton>, #key object (#f), #all-keys)
  if (object)
    singleton (object)
  else
    error ("No `object:' specified in call to make (<singleton>, ...)")
  end ;
end ;

define generic as (c :: <class>, o :: <object>) ;
define generic shallow-copy (o :: <object>) ;
define generic class-for-copy (o :: <object>) ;
  
  
  
