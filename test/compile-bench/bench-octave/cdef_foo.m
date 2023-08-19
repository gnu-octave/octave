classdef cdef_foo
  properties
    s = "";
    a = 0;
  end

  methods
    function obj = cdef_foo (s, a)
      obj.s = s;
      obj.a = a;
    end

    function c = method1 (obj, b)
       c = b + obj.a;
    end

    function a = getter1 (obj)
      a = obj.a;
    end

    function obj = setter1 (obj, a)
      obj.a = a;
    end
  end
end