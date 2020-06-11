function [r1, r2] = bug57941a (A)
  fh1 = @nested1;
  function z = nested1 (x)
    z = A * x;
  end
  fh2 = @nested2;
  function z = nested2 (x,y)
    z = A * x .* y;
  end
  r1 = fh1 (3);
  r2 = fh2 (3, 4);
end
