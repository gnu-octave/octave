function ret = method_external_function (obj, varargin)
  ret = 'method_external_function';
  if (nargin > 1)
    switch (varargin{1})
      case testpkg.testsubpkg.testclass.m
        ret = obj.method_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.s
        ret = testpkg.testsubpkg.testclass.static_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.so
        ret = obj.static_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.s2
        ret = testpkg.testsubpkg.testclass.static_external_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.s2o
        ret = obj.static_external_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.p
        ret = private_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.hs
        ret = @testpkg.testsubpkg.testclass.static_function;
      case testpkg.testsubpkg.testclass.hsv
        ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
      case testpkg.testsubpkg.testclass.hso
        ret = @obj.static_function;
      case testpkg.testsubpkg.testclass.hsvo
        ret = @(varargin) obj.static_function (varargin{:});
      case testpkg.testsubpkg.testclass.hs2
        ret = @testpkg.testsubpkg.testclass.static_external_function;
      case testpkg.testsubpkg.testclass.hs2v
        ret = @(varargin) testpkg.testsubpkg.testclass.static_external_function (varargin{:});
      case testpkg.testsubpkg.testclass.hs2o
        ret = @obj.static_external_function;
      case testpkg.testsubpkg.testclass.hs2vo
        ret = @(varargin) obj.static_external_function (varargin{:});
      case testpkg.testsubpkg.testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
