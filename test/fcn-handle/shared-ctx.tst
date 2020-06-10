## Test that multiple handles to nested functions created in the same
## context share that call stack context, but that separately created
## handles have a separate (shared) context.

%!test
%! [add10, sub10, mul10, div10] = shared_ctx (10);
%! [add100, sub100, mul100, div100] = shared_ctx (100);
%! assert (add10 (2), 12);
%! assert (add100 (20), 120);
%! assert (sub10 (4), 8);
%! assert (sub100 (40), 80);
%! assert (mul10 (5), 40);
%! assert (mul100 (50), 4000);
%! assert (div10 (4), 10);
%! assert (div100 (40), 100);
