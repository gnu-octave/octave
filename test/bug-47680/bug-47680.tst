%!shared obj
%! obj = sub_bug47680 ("foo");
%!assert <*47680> (obj.meth1 (), "foo:super:meth1:sub:meth1")
%!assert <*47680> (obj.meth2 (), "foo:super:meth2:sub:meth2")
%!assert <*47680> (obj.meth3 (), "foo:super:meth3:sub:meth3")
%!assert <*47680> (obj.meth4 (), "foo:super:meth4:sub:meth4")
