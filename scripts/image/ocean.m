function map = ocean(number)

  if(nargin == 0)
    number = 64;
  endif

  cutin = fix(number/3);
  
  dr = (number-1)/(cutin);
  r = prepad([0:dr:(number-1)],number)';
  dg = (number-1)/(2*cutin);
  g = prepad([0:dg:(number-1)],number)';
  b = [0:(number-1)]';

  map = [ r g b ]/(number-1);

endfunction
