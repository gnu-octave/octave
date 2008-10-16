function s = subsasgn (s, index, val)
  switch (index.type)
    case "()"
      if (! isnumeric (val) || iscomplex(val) ||any (val(:)) >= 2.^ s.m || 
	  any (val(:)) < 0 || any (val(:) != fix(val(:))))
	error ("subsasgn: value must be an array of real integers between 0 and 2.^m - 1");
      endif
      s.x = subsasgn (s.x, index, double (val));
    case "."
      error ("subsagn: can not set properties of a galois field directly");
  endswitch
endfunction
