function c = __sysconcat__(a,b)
  # c = __sysconcat__(a,b)
  # cell array replacement for append, used by control systems toolbox

  if(isstr(a))
    a = {a};
  endif
  if(isstr(b))
    b = {b};
  endif

  if ( ! ( is_signal_list(a)  && is_signal_list(b) ) )
    error("need cell arrays of strings");
  endif

  c = a;
  la = length(a);
  for ii=1:length(b)
    c{la+ii} = b{ii};
  endfor

endfunction

