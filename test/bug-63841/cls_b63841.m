classdef cls_b63841
  properties
    a_property = 1;
  end
  methods
    function varargout = subsref(obj, s)
      varargout = {nargout, s(1).type, 'arg3'};
    end
  end
end
