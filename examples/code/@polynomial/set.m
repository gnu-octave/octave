function s = set (p, varargin)

  if (numel (varargin) < 2 || rem (numel (varargin), 2) != 0)
    error ("@polynomial/set: expecting property/value pairs");
  endif

  s = p;
  while (numel (varargin) > 1)
    prop = varargin{1};
    val  = varargin{2};
    varargin(1:2) = [];
    if (! ischar (prop) || ! strcmp (prop, "poly"))
      error ("@polynomial/set: invalid property of polynomial class");
    elseif (! (isvector (val) && isreal (val)))
      error ("@polynomial/set: expecting the value to be a real vector");
    endif

    s.poly = val(:).';  # force row vector
  endwhile

endfunction
