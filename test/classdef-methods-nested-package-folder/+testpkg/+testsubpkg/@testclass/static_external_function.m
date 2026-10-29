function ret = static_external_function (varargin)
  ret = 'static_external_function';
  if (nargin > 0)
    switch (varargin{1})
      case testpkg.testsubpkg.testclass.s
        ret = testpkg.testsubpkg.testclass.static_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.p
        ret = private_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.hs
        ret = @testpkg.testsubpkg.testclass.static_function;
      case testpkg.testsubpkg.testclass.hsv
        ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
      case testpkg.testsubpkg.testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
