function res = AbsRel_Norm (x, x_old, AbsTol, RelTol, normcontrol, y)

  n = length (x);

  if (nargin == 5)
    y = zeros (size (x));
  elseif (nargin != 6)
    error ("OdePkg:InvalidArgument",
           "invalid number of input arguments");
  endif

  if (length (x_old) != n
      || length (y) != n)
    error ("OdePkg:InvalidArgument",
           "invalid dimensions of input arguments");
  endif
  
  if ((length (AbsTol) != 1 && length (AbsTol) != n)
      || (length (RelTol) != 1 && length (RelTol) != n))
    error ("OdePkg:InvalidArgument",
           "invalid dimensions of input arguments");
  endif
  
  sc = AbsTol + max (abs (x), abs (x_old)) .* RelTol;
  if (normcontrol)
    res = max (abs (x - y) ./ sc);
  else
    res = sqrt ((1 / n) * sum (((x - y) ./ sc).^2));
  endif

endfunction
