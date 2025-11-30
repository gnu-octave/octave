function fcn = cdef_array_break_cycles ()
  obj_array(1) = foo_value_class ();
  obj_array(2) = foo_value_class ();

  function result = nested_func (x)
    result = obj_array;
  endfunction

  fcn = @nested_func;
endfunction
