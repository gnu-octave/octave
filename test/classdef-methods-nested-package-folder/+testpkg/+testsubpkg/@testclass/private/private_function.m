function ret = private_function (varargin)
  ret = 'private_function';
  if (nargin > 0)
    switch (varargin{1})
      case testpkg.testsubpkg.testclass.s
        ret = testpkg.testsubpkg.testclass.static_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.s2
        ret = testpkg.testsubpkg.testclass.static_external_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.hs
        ret = @testpkg.testsubpkg.testclass.static_function;
      case testpkg.testsubpkg.testclass.hsv
        ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
      case testpkg.testsubpkg.testclass.hs2
        ret = @testpkg.testsubpkg.testclass.static_external_function;
      case testpkg.testsubpkg.testclass.hs2v
        ret = @(varargin) testpkg.testsubpkg.testclass.static_external_function (varargin{:});

      # for testing errors:
      # private_function cant reach local_function (not directly)
      case testpkg.testsubpkg.testclass.l
        ret = local_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.hl
        ret = @local_function;
    endswitch
  endif
endfunction
