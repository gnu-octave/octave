function r = end (obj, index_pos, num_indices)
  dv = size (obj.x);
  for i = (num_indices + 1) : length (dv)
    dv(num_indices) *= dv(i);
  endfor
  if (index_pos <= length (dv))
    r = dv (index_pos);
  elseif
    r = 1;
  endif
endfunction
