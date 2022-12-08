%!test
%! x = bug48693();
%! assert ({x{1:10}}, num2cell (zeros(1,10)))
