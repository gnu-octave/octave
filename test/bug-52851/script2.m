1; %script identifier

function r = caller (fun)
  r = fun ();
endfunction

function r = computation ()
  r = 1;
endfunction

r1 = caller (@computation);

function r = computation ()
  r = 2;
endfunction

r2 = caller (@computation);
