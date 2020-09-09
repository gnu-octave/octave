1; %script identifier

function r = caller (fun, num)
  persistent funarray
  if (isempty (fun))
    r = funarray(num).fun ();
  else
    if (isempty (funarray))
      funarray(1).fun = fun;
    else
      funarray(num).fun = fun;
    endif
  endif
endfunction

function r = computation ()
  r = 1;
endfunction

caller (@computation, 1);

r11 = caller ([], 1);

function r = computation ()
  r = 2;
endfunction

caller (@computation, 2);

r21 = caller ([], 1);
r22 = caller ([], 2);
