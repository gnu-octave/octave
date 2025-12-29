classdef class_bug60723B
  methods
    function [varargout] = subsref (obj, s)
      varargout = num2cell (repelem(nargout, max(nargout, 1)));
    end
    function n = numel (obj, idx)
      n = 2;
    end
  end
end

