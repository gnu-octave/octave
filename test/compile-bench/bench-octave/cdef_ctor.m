function cdef_ctor (n)
  for i = 1:n
    obj = cdef_foo ("asd", i);
  end
end

% bugg
function cdef_ctor1 (n)
  for i = 1:n
    obj = cdef_ctor1 ("asd", i);
  end
end
