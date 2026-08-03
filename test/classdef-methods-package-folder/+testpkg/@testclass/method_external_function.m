function ret = method_external_function (obj, varargin)
  ret = 'method_external_function';
  if (nargin > 1)
    switch (varargin{1})
      case testpkg.testclass.m
        ret = obj.method_function (varargin{2:end});
      case testpkg.testclass.s
        ret = testpkg.testclass.static_function (varargin{2:end});
      case testpkg.testclass.so
        ret = obj.static_function (varargin{2:end});
      case testpkg.testclass.s2
        ret = testpkg.testclass.static_external_function (varargin{2:end});
      case testpkg.testclass.s2o
        ret = obj.static_external_function (varargin{2:end});
      case testpkg.testclass.p
        ret = private_function (varargin{2:end});
      case testpkg.testclass.hs
        ret = @testpkg.testclass.static_function;
      case testpkg.testclass.hsv
        ret = @(varargin) testpkg.testclass.static_function (varargin{:});
      case testpkg.testclass.hso
        ret = @obj.static_function;
      case testpkg.testclass.hsvo
        ret = @(varargin) obj.static_function (varargin{:});
      case testpkg.testclass.hs2
        ret = @testpkg.testclass.static_external_function;
      case testpkg.testclass.hs2v
        ret = @(varargin) testpkg.testclass.static_external_function (varargin{:});
      case testpkg.testclass.hs2o
        ret = @obj.static_external_function;
      case testpkg.testclass.hs2vo
        ret = @(varargin) obj.static_external_function (varargin{:});
      case testpkg.testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
