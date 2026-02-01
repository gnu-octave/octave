## load anonymous function handle that does not require workspace context
%!testif HAVE_ZLIB <*67964>
%! ## file saved in MATLAB on amd64 using the following commands:
%! ## f = @() x + 1;
%! ## save ('foo1.mat', 'f', '-v7');
%! l = load ("foo1.mat");
%! assert (isfield (l, "f"));
%! assert (isa (l.f, "function_handle"));
%! assert (l.f (1), 2);

## load anonymous function handle with associated workspace context
%!testif HAVE_ZLIB <*67964>
%! ## file saved in MATLAB on amd64 using the following commands:
%! ## a = 3;
%! ## g = @() x + a;
%! ## save ('foo2.mat', 'g', '-v7');
%! l = load ("foo2.mat");
%! assert (isfield (l, "g"));
%! assert (isa (l.g, "function_handle"));
%! assert (l.g (1), 4);
