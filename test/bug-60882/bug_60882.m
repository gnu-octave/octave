function retval = bug_60882 ()

  job.foobar = {};

  foobar off

  retval = 42;

endfunction

function foobar (opt)
  assert (opt, 'off');
endfunction
