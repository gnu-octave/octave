classdef bug65037_derived < bug65037_base
  methods
    function obj = bug65037_derived (varargin)
      obj@bug65037_base (varargin{:});
    end
  end
end
