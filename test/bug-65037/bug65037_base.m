classdef bug65037_base
  properties
    ctor_nargin;
  end
  methods
    function obj = bug65037_base (varargin)
      obj.ctor_nargin = nargin;
    end
  end
end
