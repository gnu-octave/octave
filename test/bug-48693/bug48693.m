classdef bug48693 < handle
  methods
    function varargout = subsref (x, idx)
      varargout = num2cell (zeros (size (idx(1).subs{1})));
    end
  end
end
