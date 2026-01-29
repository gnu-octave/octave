## load anonymous function handle that does not require workspace context
%!test <*67964>
%! l = load ("foo1.mat");  # file saved in MATLAB
%! assert (isfield (l, "f"));
%! assert (isa (l.f, "function_handle"));
%! assert (l.f (1), 2);

## load anonymous function handle with associated workspace context
%!test <*67964>
%! l = load ("foo2.mat");  # file saved in MATLAB R2025b
%! assert (isfield (l, "g"));
%! assert (isa (l.g, "function_handle"));
%! assert (l.g (1), 4);
