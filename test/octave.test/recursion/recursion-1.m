1;
function y = f (x)
  if (x == 1)
    y = x;
    return;
  else
    y = x * f (x-1);
  endif
endfunction
f (5)
