function r = bug64739_args (a, b)
  arguments
    a double
    b double = 7
  endarguments
  bug64739_noop hello
  r = a + b;
endfunction
