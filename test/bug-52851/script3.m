1; %script identifier

if (flag)
  function r = computation ()
    r = 1;
  endfunction

  r = computation ();
else
  function r = computation ()
    r = 2;
  endfunction

  r = computation ();
endif
