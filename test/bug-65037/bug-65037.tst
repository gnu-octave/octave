%!test <65037>
%! base_1 = bug65037_base ('base class arg');
%! derived = bug65037_derived ('derived class arg');
%! base_2 = bug65037_base (base_1);
%! base_3 = bug65037_base (derived);
%! assert (base_1.ctor_nargin, 1);
%! assert (derived.ctor_nargin, 1);
%! assert (base_2.ctor_nargin, 1);
%! assert (base_3.ctor_nargin, 1);
