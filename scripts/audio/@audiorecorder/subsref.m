function value = subsref(recorder, idx)
  if (isempty (idx))
    error ("audiorecorder: missing index");
  endif
  if (strcmp(idx(1).type, "."))
    field = idx.subs;
    value = get(recorder, field);
  else
    error ("audiorecorder: invalid subscript file")
  endif
endfunction