function A = for_loop_matselfmul (sq)
  A = zeros (size (sq));

  cols = size (sq, 2);
  rows = size (sq, 1);

  assert (cols == rows);

  n = cols;

  for i=1:n
    for j=1:n
      for k=1:n
        A(i,j) = A(i,j)+sq(i,k)*sq(k,j);
      end
    end
  end
end
