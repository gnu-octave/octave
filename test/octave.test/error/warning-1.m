function g () warning ("foo") endfunction
function f () g (); endfunction
f ();
