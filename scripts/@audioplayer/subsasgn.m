function value = subsasgn(player, idx, rhs)
  if (isempty (idx))
    error ("audioplayer: missing index");
  endif
  if (strcmp(idx(1).type, "."))
    field = idx.subs;
    set(player, field, rhs);
    value = player;
  else
    error ("audioplayer: invalid subscript type");
  endif
endfunction