classdef cdef_foo < handle
  properties
    msg = "";
    val = 1;
  end
  methods
    function f = cdef_foo(msg)
        global cdef_foo_ctor_cnt = 0;
        f.msg = msg;
        cdef_foo_ctor_cnt++;
    end
    function delete (self)
      global cdef_foo_dtor_cnt = 0;
      __printf_assert__ ("%d %s ", ++cdef_foo_dtor_cnt, self.msg);
    endfunction
    function c = plus (a, b)
        c = cdef_foo (strcat("sum", a.msg, b.msg));
        c.val = a.val + b.val;
    end
  endmethods
end
