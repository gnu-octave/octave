function p = polyreduce(p)
#polyreduce(c)
#Reduces a polynomial coefficient vector to a minimum number of terms,
#i.e. it strips off any leading zeros.
#
#SEE ALSO: poly, roots, conv, deconv, residue, filter, polyval, polyvalm,
#          polyderiv, polyinteg

# Author:
#  Tony Richardson
#  amr@mpl.ucsd.edu
#  June 1994

  index = find(p==0);

  index = find(index == 1:length(index));

  if (length(index) == 0)
    return;
  endif

  if(length(p)>1)
    p = p(index(length(index))+1:length(p));
  endif

  if(length(p)==0)
    p = 0;
  endif
endfunction
