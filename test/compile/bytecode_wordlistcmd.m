function bytecode_wordlistcmd ()
  foo A B C;

  bar QWE;

  boz
  boz A
  boz A B
end


function foo (a,b,c)
  __printf_assert__ ("%s ", a);
  __printf_assert__ ("%s ", b);
  __printf_assert__ ("%s ", c);
end

function [a b] = bar (c)
  __printf_assert__ ("%s ", c);
  a = 1;
  b = 2;
end

function boz (a,b,c)
end
