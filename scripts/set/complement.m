function y = complement (a, b)

# usage: complement (a, b)
#
# Returns the elements of set b that are not in set a.
#
# See - create_set, union, intersection

  if (nargin != 2)
    usage ("complement(a,b)");
  endif

  if (isempty (a))
    y = create_set(b);
  elseif (isempty (b))
    y = [];
  else
    a = create_set (a);
    b = create_set (b);
    yindex = 1;
    y = zeros (1, length (b));
    for index = 1:length (b)
      if (all (a != b (index)))
        y(yindex++) = b(index);
      endif
    endfor
    y = y(1:(yindex-1));
  endif

endfunction

