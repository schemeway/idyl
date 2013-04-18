// ---------------------------------------------------------------------- //
// FICHIER               : error.dylan                                    //
// DATE DE CREATION      : Mon May 29 10:07:19 1995                       //
// DERNIERE MODIFICATION : Thu Jun  8 11:15:13 1995                       //
// ---------------------------------------------------------------------- //
// Copyright (c) 1995 Dominique Boucher                                   //
// ---------------------------------------------------------------------- //
// The condition protocol ...                                             //
// ---------------------------------------------------------------------- //

define constant error-message =
  method (cond :: <condition>)
    select (cond by instance?)
      <simple-error> => "Error" ;
      <type-error>   => "Type error" ;
      <warning>      => "Warning" ;
      <restart>      => "Restart" ;
      otherwise      => "Condition" ;
    end
  end ;

define constant $default-message = "(no message specified)";

define constant print-signal-prolog = 
  method (cond :: <condition>) printf ("// [* %s: ", error-message (cond)) end ;
define constant print-signal-epilog = 
  method () printf (" *]\n") end ;

define constant abort = 
  method () error (make (<abort>)) end ;

define method default-handler (cond :: <abort>) \
  printf ("// [* ABORTING ... returning to top-level *]\n") ;
end ;

define method default-handler (cond :: <condition>) #f end ;
define method default-handler (cond :: <serious-condition>) abort() end ;

define method default-handler (cond :: <simple-error>) \
  let msg = if (slot-initialized? (cond, condition-format-string))
 	       cond.condition-format-string
	    end ;
  let args = if (slot-initialized? (cond, condition-format-arguments))
                cond.condition-format-arguments
	     else #() end ;
  print-signal-prolog (cond) ;
  if (msg)  simple-apply (printf, pair (msg, args)) 
  else      printf ($default-message, error-message (cond))
  end ;
  print-signal-epilog () ;
  abort () ;
  end ;

define method default-handler (cond :: <simple-warning>) 
  let msg = if (slot-initialized? (cond, condition-format-string))
 	       cond.condition-format-string
            end ;
  let args = if (slot-initialized? (cond, condition-format-arguments))
                cond.condition-format-arguments
             else #() end ;
  print-signal-prolog (cond) ;
  if (msg)  simple-apply (printf, pair (msg, args)) 
  else      printf ($default-message, error-message (cond))
  end ;
  print-signal-epilog () ;
  values (#f) ;
  end ;

define method default-handler (cond :: <type-error>) 
  print-signal-prolog (cond) ;
  printf ("expected type: `%=', value: `%='", 
          type-error-type(cond), type-error-value (cond)) ;
  print-signal-epilog () ;
  abort () ;
end ;
