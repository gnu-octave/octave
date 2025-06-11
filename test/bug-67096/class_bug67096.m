classdef class_bug67096
  methods
    function r = get_struct (obj)
      r = struct ('data', num2cell (nargout:nargout+4));
    end
    function r = get_cell (obj)
      r = num2cell (nargout:nargout+4);
    end
  end
end
