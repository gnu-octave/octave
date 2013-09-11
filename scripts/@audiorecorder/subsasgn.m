function value = subsasgn(recorder, idx, rhs)
  if (isempty (idx))
    error ("audiorecorder: missing index");
  endif
  if (strcmp(idx(1).type, "."))
    field = idx.subs;
    set(recorder, field, rhs);
    value = recorder;
  else
    error ("audiorecorder: invalid subscript type");
  endif
endfunction