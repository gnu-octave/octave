function bug_64620_2()
  % Two levels of nested nesting
  global cdef_alive_objects_cntr = 0;
  cdef_alive_objects_cntr = 0;

  h = sub_returns_nested_fn2;
  h ();
  h = 3; % Clear handle, triggers dtors

  assert (cdef_alive_objects_cntr == 0);
end

function h1 = sub_returns_nested_fn2
  c2 = cdef_counts_self;

  function h2 = nested_fn1
    c3 = cdef_counts_self;

    function nested_fn2
      c4 = cdef_counts_self;
    end

    h2 = @nested_fn2;
  end

  h1 = nested_fn1 ();
end

