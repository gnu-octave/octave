function [m_exist, n_exist] = ntest53027a ()
  global x
  x = 3;
  n_exist = nest ();
  m_exist = exist ("x", 'var');
  function n_exist = nest ()
    x = 1;
    clear x
    n_exist = exist ("x", "var");
  endfunction
endfunction
