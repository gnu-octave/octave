function y = intersection(a,b)

# usage: intersection(a,b)
#
# Returns the intersection of sets a and b.
#
# See - create_set, union, complement

  if (nargin != 2)
    usage ("intersection(a,b)");
  endif

  if(isempty(a) || isempty(b))
    y = [];
    return;
  endif

  a = create_set(a);
  b = create_set(b);

  if(length(a) < length(b))
    yindex = 1;
    y = zeros(1,length(a));
    for index = 1:length(a)
      if(any(b == a(index)))
        y(yindex++) = a(index);
      endif
    endfor
  else
    yindex = 1;
    y = zeros(1,length(b));
    for index = 1:length(b)
      if(any(a == b(index)))
        y(yindex++) = b(index);
      endif
    endfor
  endif

  y = y(1:(yindex-1));

endfunction
