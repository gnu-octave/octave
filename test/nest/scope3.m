## scope3.m
function scope3
  C;
  function A
    B;
    function B
      E;
    endfunction
    function E
    endfunction
  endfunction

  function C
    D;
    function D
      A;
    endfunction
  endfunction
endfunction
