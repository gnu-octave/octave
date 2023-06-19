% TODO: When clear and dynamic stack works, test
% that behavoiur is the same when clearing globals
% in another function while they are on the stack in the
% caller.

function bytecode_global_1 ()
  % General test. a and b are also read and verified in test .tst file
  global a b
  __printf_assert__ ("%s ", class (a)); 
  __printf_assert__ ("%d ", size (a));

  __printf_assert__ ("%d ", length (who ('global','a')));
  __printf_assert__ ("%d ", length (who ('global','b')));
  __printf_assert__ ("%d ", isglobal ('a'));
  __printf_assert__ ("%d ", isglobal ('b'));

  a = 1;
  __printf_assert__ ("%d ", a);
  b = 2;
  __printf_assert__ ("%d ", b);

  a = b;
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);
  b = 100;
  a = 3 * b + max (a, b);
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  % Test that we can make globals in subfunctions
  global e %sub1 needs a global "e"
  e = 11;
  sub1 (1);

  __printf_assert__ ("%d ", isglobal ('a'));
  __printf_assert__ ("%d ", isglobal ('b'));
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  sub1 (0);

  __printf_assert__ ("%d ", isglobal ('a'));
  __printf_assert__ ("%d ", isglobal ('b'));
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  % Declare global, clear it, use identifier as local, declare
  % it as global ...
  global c
  __printf_assert__ ("%d ", length (who ('global','c')));
  clear global c;
  __printf_assert__ ("%d ", length (who ('global','c')));

  c = 2;
  __printf_assert__ ("%d ", c);
  __printf_assert__ ("%s ", class(c));

  global c
  __printf_assert__ ("%d ", c);
  __printf_assert__ ("%s ", class(c));

  % Subassign
  global f
  f = [1 2 3 4 5];
  f(3) = 6;
  f(1) = 11;
  __printf_assert__ ("%d ", f);
  __printf_assert__ ("%s ", class(f));
  __printf_assert__ ("%d ", size (f));

  % Multiassign
  global g h
  [g h f] = returns3 ();
  __printf_assert__ ("%d ", g);
  __printf_assert__ ("%s ", class(g));
  __printf_assert__ ("%d ", size (g));
  __printf_assert__ ("%d ", h);
  __printf_assert__ ("%s ", class(h));
  __printf_assert__ ("%d ", size (h));
  __printf_assert__ ("%d ", f);
  __printf_assert__ ("%s ", class(f));
  __printf_assert__ ("%d ", size (f));

  % Init expression
  global k = 3;
  __printf_assert__ ("%d ", k);
  __printf_assert__ ("%s ", class(k));
  __printf_assert__ ("%d ", size (k));

  global l = 4 m = max (10,9) n = [2,3] o = k;
  __printf_assert__ ("%d ", l);
  __printf_assert__ ("%s ", class(l));
  __printf_assert__ ("%d ", size (l));
  __printf_assert__ ("%d ", m);
  __printf_assert__ ("%s ", class(m));
  __printf_assert__ ("%d ", size (m));
  __printf_assert__ ("%d ", n);
  __printf_assert__ ("%s ", class(n));
  __printf_assert__ ("%d ", size (n));
  __printf_assert__ ("%d ", o);
  __printf_assert__ ("%s ", class(o));
  __printf_assert__ ("%d ", size (o));

  % Init expression for existing local
  p = 2;
  global p = 3;
  __printf_assert__ ("%d ", p);
  __printf_assert__ ("%s ", class(p));
  __printf_assert__ ("%d ", size (p));
  % q created in caller already
  global q = 4;
  __printf_assert__ ("%d ", q);
  __printf_assert__ ("%s ", class(q));
  __printf_assert__ ("%d ", size (q));

  % Reinit does nothing
  global r = 7
  global r = 8
  __printf_assert__ ("%d ", r);
  __printf_assert__ ("%s ", class(r));
  __printf_assert__ ("%d ", size (r));

  clear global c
  __printf_assert__ ("%d ", length (who ('global','c')));

  clear global d
  clear global e
  clear global f
  clear global g
  clear global h
  clear global k
  clear global l
  clear global m
  clear global n
  clear global o
  clear global p
  clear global r
end

function [q w e] = returns3()
  q = 11;
  w = 22;
  e = 33;
end


function sub1(make_global)
  % Already defined local, later declared global  
  d = 3;
  __printf_assert__ ("%d ", length(who('global','d')));
  global d
  __printf_assert__ ("%d ", length(who('global','d')));
  __printf_assert__ ("%d ", d);
  __printf_assert__ ("%s ", class(d));
  __printf_assert__ ("%d ", size (d));
  d = [1 2];
  __printf_assert__ ("%d ", d);
  __printf_assert__ ("%s ", class(d));
  __printf_assert__ ("%d ", size (d));

  % Already defined local, later declared global,
  % but with the global already with a value from the caller
  e = 4;
  __printf_assert__ ("%d ", length(who('global','e')));
  global e
  __printf_assert__ ("%d ", length(who('global','e')));
  __printf_assert__ ("%d ", e);
  __printf_assert__ ("eclass:%s ", class(e));
  __printf_assert__ ("%d ", size (e));
  e = [3 4];
  __printf_assert__ ("%d ", e);
  __printf_assert__ ("%s ", class(e));
  __printf_assert__ ("%d ", size (e));
  

  % Conditionally global a and b
  if make_global
    global a
    global b

    __printf_assert__ ("%d ", a);
    __printf_assert__ ("%d ", b);
  end

  __printf_assert__ ("%d ", isglobal ('a'));
  __printf_assert__ ("%d ", isglobal ('b'));
  __printf_assert__ ("%d ", length(who('global','a')));
  __printf_assert__ ("%d ", length(who('global','b')));

  a = 3;
  b = 4;

  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", b);

  if make_global
    a = 5;
    b = 6;
  end
end

