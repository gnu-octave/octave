1;
function y = f (x)
  if (x == 1)
    y = x;
    return;
  else
    y = f (x-1) * x;
  endif
endfunction
f (5)
