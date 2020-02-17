function fh = nst1 (xval)
  if (nargin > 0)
    x = xval;
  end
  fh = @nst;
  function r = nst (xval)
    if (nargin > 0)
      x = xval;
    end
    r = x;
  end
end
