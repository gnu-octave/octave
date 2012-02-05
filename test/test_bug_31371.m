%!test
%! % Work around MATLAB bug where f(x)(y) is invalid syntax
%! % (This bug does not apply to Octave)
%! C = @(f,x) f(x);
%! C2 = @(f,x,y) f(x,y);
%! 
%! % Church Booleans
%! T = @(t,f) t;
%! F = @(t,f) f;
%! 
%! % Church Numerals in MATLAB
%! Zero  = @(f,x) x;
%! One   = @(f,x) f(x);
%! Two   = @(f,x) f(f(x));
%! Three = @(f,x) f(f(f(x)));
%! Four  = @(f,x) f(f(f(f(x))));
%! 
%! % Arithmetic Operations
%! Inc = @(a) @(f,x) f(a(f,x)); % Increment
%! Add = @(a,b) @(f,x) a(f,b(f,x));
%! Mult = @(a,b) @(f,x) a(@(x) b(f,x),x);
%! Dec = @(a) @(f,x) C(a(@(g) @(h) h(g(f)), @(u) x), @(u) u); % Decrement
%! Sub = @(a,b) b(Dec, a);
%! 
%! % Renderer - Convert church numeral to "real" number
%! render = @(n) n(@(n) n+1,0);
%! 
%! % Predicates
%! iszero = @(n) n(@(x) F, T);
%! 
%! % Y combinator implements recursion
%! Y = @(f) C(@(g) f(@(x) C(g(g), x)), ...
%!            @(g) f(@(x) C(g(g), x)));
%! 
%! Factorial = Y(@(f) @(n) C(C2(iszero(n), ...
%!               @(d) One, @(d) Mult(n, f(Dec(n)))),0));
%! 
%! assert (render(Factorial(Two), 2))
%! assert (render(Factorial(Three), 3))
%! assert (render(Factorial(Four), 12))
