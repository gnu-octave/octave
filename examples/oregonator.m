## The `oregonator'.

function dx = oregonator (x, t)

  dx = zeros (3, 1);

  dx(1) = 77.27*(x(2) - x(1)*x(2) + x(1) - 8.375e-06*x(1)^2);
  dx(2) = (x(3) - x(1)*x(2) - x(2)) / 77.27;
  dx(3) = 0.161*(x(1) - x(3));

end
