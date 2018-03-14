%!test <49379>
%! a1 = 1;
%! a2 = 2;
%! obj = class_bug49379 (a1, a2);
%! assert (obj.ctor_arg_names (), {"a1", "a2"})
%! meth (obj, a2, a1);
%! assert (obj.meth_arg_names (), {"obj", "a2", "a1"})
%! obj.meth (a2, a1);
%! assert (obj.meth_arg_names (), {"obj", "a2", "a1"})
