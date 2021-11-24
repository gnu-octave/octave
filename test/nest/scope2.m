## scope2.m
function scope2
  C;
  function A
    B;
    function B
      D;
    endfunction
  endfunction

  function C
    D;
    function D
      A;
    endfunction
  endfunction
endfunction
