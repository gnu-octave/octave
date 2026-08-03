function ret = private_function (varargin)
  ret = 'private_function';
  if (nargin > 0)
    switch (varargin{1})
      case testclass.s
        ret = testclass.static_function (varargin{2:end});
      case testclass.hs
        ret = @testclass.static_function;
      case testclass.hsv
        ret = @(varargin) testclass.static_function (varargin{:});

      # for testing errors:
      # private_function cant reach local_function (not directly)
      case testclass.l
        ret = local_function (varargin{2:end});
      case testclass.hl
        ret = @local_function;
    endswitch
  endif
endfunction
