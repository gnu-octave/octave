function ret = private_function (varargin)
  ret = 'private_function';
  if (nargin > 0)
    switch (varargin{1})
      case testpkg.testclass.s
        ret = testpkg.testclass.static_function (varargin{2:end});
      case testpkg.testclass.s2
        ret = testpkg.testclass.static_external_function (varargin{2:end});
      case testpkg.testclass.hs
        ret = @testpkg.testclass.static_function;
      case testpkg.testclass.hsv
        ret = @(varargin) testpkg.testclass.static_function (varargin{:});
      case testpkg.testclass.hs2
        ret = @testpkg.testclass.static_external_function;
      case testpkg.testclass.hs2v
        ret = @(varargin) testpkg.testclass.static_external_function (varargin{:});

      # for testing errors:
      # private_function cant reach local_function (not directly)
      case testpkg.testclass.l
        ret = local_function (varargin{2:end});
      case testpkg.testclass.hl
        ret = @local_function;
    endswitch
  endif
endfunction
