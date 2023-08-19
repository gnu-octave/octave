function cdef_method1 (n)
  obj = cdef_foo ("qwe", 123);
  for i = 1:n
    obj.method1 (2);
  end
end