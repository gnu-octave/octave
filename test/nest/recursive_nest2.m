## recursive_nest2.m
function x = recursive_nest2 ()
  x = B (20);
  function v = B (n)
    Y = 0;
    BB (n);
    C;
    v = Y;
    function BB (m)
      if (m > 0)
        Y = Y + 1;
        BB(m - 1);
        C;
      end
    endfunction
  endfunction

  function C
    Y = 0;
  endfunction
endfunction
