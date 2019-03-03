function fh = nst3 (gval)
  global g
  if (nargin > 0)
    g = gval;
  end
  fh = @nst;
  function r = nst (gval)
    if (nargin > 0)
      g = gval;
    end
    r = g;
  end
end
