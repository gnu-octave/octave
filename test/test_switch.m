%% Automatically generated from DejaGNU files

%% test/octave.test/switch/switch-1.m
%!test
%! a = 1;
%! b = 2;
%! c = 3;
%! 
%! switch 0 case 1 x = a; case 2 x = b; otherwise x = c; endswitch
%! switch 1 case 1 y = a; case 2 y = b; otherwise y = c; endswitch
%! switch 2 case 1 z = a; case 2 z = b; otherwise z = c; endswitch
%! switch 3 case 1 p = a; case 2 p = b; otherwise p = c; endswitch
%! 
%! assert(x == c && y == a && z == b && p == c);

%% test/octave.test/switch/switch-2.m
%!test
%! a = 1;
%! b = 2;
%! c = 3;
%! 
%! x = zeros (1, 4);
%! 
%! k = 1;
%! 
%! for i = 0:3
%! switch (i)
%! case a
%! x(k) = a;
%! case b
%! x(k) = b;
%! otherwise
%! x(k) = c;
%! endswitch
%! k++;
%! endfor
%! 
%! assert(all (x == [3, 1, 2, 3]));

%% test/octave.test/switch/switch-3.m
%!test
%! a = 1;
%! b = 2;
%! c = 3;
%! 
%! x = zeros (1, 4);
%! 
%! k = 1;
%! 
%! for i = 0:3
%! switch (i)
%! case a
%! x(k) = a;
%! endswitch
%! k++;
%! endfor
%! 
%! assert(all (x == [0, 1, 0, 0]));

%% test/octave.test/switch/switch-4.m
%!error <syntax error> eval("switch endswitch");

%% test/octave.test/switch/switch-5.m
%!error <syntax error> eval("switch case endswitch");

%% test/octave.test/switch/switch-6.m
%!error <syntax error> eval("switch 1 default 1; endswitch");

