classdef class_bug52614B < class_bug52614A
  properties
    b
  end

  methods
    function obj = class_bug52614B ()
      obj = obj@class_bug52614A ();
    end

    function foo (obj)
      foo@class_bug52614A (obj);
      obj.b = 2;
    end
  end
end
