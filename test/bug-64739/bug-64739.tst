## A name followed by a class in a properties block is a declaration, not a
## command.
%!test <*64739>
%! obj = class_bug64739 ();
%! assert ({obj.a, obj.b, obj.c, obj.d, double(obj.e), obj.f},
%!         {[], 5, 1, 2, 3, [0, 0]});

## A declaration on the same line as the keyword is a declaration too.
%!test <*64739>
%! obj = class_bug64739 ();
%! assert (obj.g, 4);

%!test <*64739>
%! assert (bug64739_args_oneline (3), 3);

## Command syntax still works in a method after the properties blocks.
%!test <*64739>
%! obj = class_bug64739 ();
%! assert (after_block (obj), 11);

## The same holds in an arguments block, and in the function body after it.
%!test <*64739>
%! assert (bug64739_args (1, 2), 3);
