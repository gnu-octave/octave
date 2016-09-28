function pout = set (p, varargin)

  if (numel (varargin) < 2 || rem (numel (varargin), 2) != 0)
    error ("@polynomial/set: expecting PROPERTY/VALUE pairs");
  endif

  pout = p;
  while (numel (varargin) > 1)
    prop = varargin{1};
    val  = varargin{2};
    varargin(1:2) = [];
    if (! ischar (prop) || ! strcmp (prop, "poly"))
      error ("@polynomial/set: invalid PROPERTY for polynomial class");
    elseif (! (isreal (val) && isvector (val)))
      error ("@polynomial/set: VALUE must be a real vector");
    endif

    pout.poly = val(:).';  # force row vector
  endwhile

endfunction
