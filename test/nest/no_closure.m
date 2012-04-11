# no_closure.m
function no_closure (n)
  if n == 0
    x = @no_closure;
  else
    f = @no_closure;
  endif

  function f
  endfunction
endfunction
