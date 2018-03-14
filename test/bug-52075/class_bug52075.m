classdef class_bug52075
  methods
    function ret = duplicate (self)
      ret = 'method';
    end
    function ret = call_local_dup (self)
      ret = duplicate ();
    end
  end
end

function ret = duplicate
  ret = 'local_foo';
end
