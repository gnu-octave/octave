## scope1.m
function scope1 (n)
  value = n;
  if (value)
    C;
  end
  function A
    B;
    function B
      scope1 (0);
    endfunction
  endfunction

  function C
    D;
    function D
      A;
    endfunction
  endfunction
endfunction
