function ret = static_external_function (varargin)
  ret = 'static_external_function';
  if (nargin > 0)
    switch (varargin{1})
      case testclass.s
        ret = testclass.static_function (varargin{2:end});
      case testclass.p
        ret = private_function (varargin{2:end});
      case testclass.hs
        ret = @testclass.static_function;
      case testclass.hsv
        ret = @(varargin) testclass.static_function (varargin{:});
      case testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
