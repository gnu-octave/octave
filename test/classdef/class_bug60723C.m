classdef class_bug60723C
  methods
    function [varargout] = subsref (obj, s)
      varargout = num2cell (repelem(nargout, max(nargout, 1)));
    end
    function ind = end (obj, k, n)
      ind = 4;
    end
  end
end

