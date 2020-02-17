classdef class_bug52614A < handle
  properties
    a
  end

  methods
    function obj = class_bug52614A ()
      obj.foo ();
    end

    function foo (obj)
      obj.a = 1;
    end
  end
end
