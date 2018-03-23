function [m_exist, n_exist] = ntest53027b ()
  global x
  x = 3;
  n_exist = nest ();
  m_exist = exist ("x", 'var');
  function n_exist = nest ()
    ## The clear statement should operate on the variable in the
    ## parent scope even though there is no explicit varabiable
    ## reference in the code (the clear function just sees a string
    ## containing the name of the variable and the parser (correctly)
    ## does not treat this as a special case.
    clear x
    n_exist = exist ("x", "var");
  endfunction
endfunction
