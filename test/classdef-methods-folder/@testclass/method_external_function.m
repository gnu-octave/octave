function ret = method_external_function (obj, varargin)
  ret = 'method_external_function';
  if (nargin > 1)
    switch (varargin{1})
      case testclass.m
        ret = obj.method_function (varargin{2:end});
      case testclass.s
        ret = testclass.static_function (varargin{2:end});
      case testclass.so
        ret = obj.static_function (varargin{2:end});
      case testclass.s2
        ret = testclass.static_external_function (varargin{2:end});
      case testclass.s2o
        ret = obj.static_external_function (varargin{2:end});
      case testclass.p
        ret = private_function (varargin{2:end});
      case testclass.hs
        ret = @testclass.static_function;
      case testclass.hsv
        ret = @(varargin) testclass.static_function (varargin{:});
      case testclass.hso
        ret = @obj.static_function;
      case testclass.hsvo
        ret = @(varargin) obj.static_function (varargin{:});
      case testclass.hs2
        ret = @testclass.static_external_function;
      case testclass.hs2v
        ret = @(varargin) testclass.static_external_function (varargin{:});
      case testclass.hs2o
        ret = @obj.static_external_function;
      case testclass.hs2vo
        ret = @(varargin) obj.static_external_function (varargin{:});
      case testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
