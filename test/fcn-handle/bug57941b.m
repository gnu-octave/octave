function [fh1, fh2] = bug57941b (A)
  fh1 = @nested1;
  function z = nested1 (x)
    z = A * x;
  end
  fh2 = @nested2;
  function z = nested2 (x,y)
    z = A * x .* y;
  end
end
