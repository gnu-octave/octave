%!shared A
%! A = [ ...
%!  1.00003+0.00000i, -0.68292+0.26406i, -0.57670+0.25291i,  0.34986+0.03335i;
%! -0.68292-0.26406i,  0.94539+0.00000i,  0.69006-0.03967i, -0.43847+0.38619i;
%! -0.57670-0.25291i,  0.69006+0.03967i,  0.95260+0.00000i, -0.34734+0.40728i;
%!  0.34986-0.03335i, -0.43847-0.38619i, -0.34734-0.40728i,  0.98356+0.00000i];
%!

%!test <*49904>
%! [Ainv, rcond] = inv (A);
%! assert (ishermitian (Ainv));
%! assert (A * Ainv, eye (4, 'double'), eps ('double') / rcond)

%!test <*49904>
%! [Ainv, rcond] = inv (single (A));
%! assert (ishermitian (Ainv));
%! assert (A * Ainv, eye (4, 'single'), eps ('single') / rcond)
