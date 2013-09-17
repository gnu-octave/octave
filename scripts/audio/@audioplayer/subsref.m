function value = subsref(player, idx)
  if (isempty (idx))
    error ("audioplayer: missing index");
  endif
  if (strcmp(idx(1).type, "."))
    field = idx.subs;
    value = get(player, field);
  else
    error ("audioplayer: invalid subscript file")
  endif
endfunction