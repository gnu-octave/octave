function ret = static_external_function (varargin)
  ret = 'static_external_function';
  if (nargin > 0)
    switch (varargin{1})
      case testpkg.testclass.s
        ret = testpkg.testclass.static_function (varargin{2:end});
      case testpkg.testclass.p
        ret = private_function (varargin{2:end});
      case testpkg.testclass.hs
        ret = @testpkg.testclass.static_function;
      case testpkg.testclass.hsv
        ret = @(varargin) testpkg.testclass.static_function (varargin{:});
      case testpkg.testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
