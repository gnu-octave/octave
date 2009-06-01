function r = end (obj, index_pos, num_indices)

  if ( num_indices!=1 )
    error ("polynomial object may only have one index")
  endif
  
  if ( (index_pos<1) || (index_pos>length(obj.poly)) )
    error ("subscript out of range")
  end

  r = length(obj.poly);

endfunction
