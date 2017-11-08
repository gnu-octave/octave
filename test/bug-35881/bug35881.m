function bug35881 (flag = 0)
  persistent a;
  global _tstvar_;

  if (isempty (a))
    a = 1;
  endif

  _tstvar_.init1 = subf ();
  _tstvar_.a1 = a;
  ##ML:fprintf ('a=%d\n', a);

  switch (flag)
    case 0
      clear subf;
    case 1
      clear functions;
    case 2
      clear all;
  endswitch

  _tstvar_.init2 = subf ();
  _tstvar_.a2 = a;
  ##ML:fprintf ('a=%d\n', a);
  ##ML:fprintf ('------\n\n');
endfunction

function retval = subf ()
  persistent x;

  retval = false;
  ##ML:fprintf ('subf: ');
  if (isempty (x))
    x = 1;
    retval = true;
    ##ML:fprintf ('INIT, ');
  endif

endfunction

## Expected results from Matlab 2016A
## Uncomment "##ML:"
#{
>> clear all;  bug35881 (0)
subf: INIT, a=1
subf: a=1
------

>> clear all;  bug35881 (1)
subf: INIT, a=1
subf: a=1
------

>> clear all;  bug35881 (2)
subf: INIT, a=1
subf: Reference to a cleared variable a.
Error in test_clear_inside_function (line 13)
subf ();  fprintf ('a=%d\n', a);
#}

