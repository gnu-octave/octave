function g () error ("foo"); endfunction
function f () g (); endfunction
f ();
