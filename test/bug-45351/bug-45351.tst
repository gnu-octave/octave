%!shared obj, h1, h2, h3
%! obj = bug45351 ();
%! [h1, h2, h3] = obj.get_handles ();
%!assert <*45351> (obj.bar (1), 'bug45351.foo: 1')
%!assert <*45351> (obj.bar (2), 'bug45351.foo: 2')
%!assert <*45351> (obj.bar (3), 'bug45351.foo: 3')
%!assert <*45351> (h1 (obj, 1), 'bug45351.foo: 1')
%!assert <*45351> (h2 (obj, 2), 'bug45351.foo: 2')
%!assert <*45351> (h3 (obj, 3), 'bug45351.foo: 3')
%!error (obj.h1 (1))
%!error (obj.h2 (2))
%!error (obj.h3 (3))
