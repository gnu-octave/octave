function map = gray(number)

  if(nargin == 0)
    number = 64;
  endif

  gr = [0:(number-1)]';
  

  map = [ gr gr gr ]/(number-1);

endfunction
