function fh = nst2 (pval)
  persistent p
  if (nargin > 0)
    p = pval;
  end
  fh = @nst;
  function r = nst (pval)
    if (nargin > 0)
      p = pval;
    end
    r = p;
  end
end
