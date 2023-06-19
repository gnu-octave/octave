function bytecode_varargin (b, varargin)
  __printf_assert__ ("%d ", varargin{:});
  __printf_assert__ ("%d ", size (varargin))

  varg1 (1);
  varg1 (1,2,3,4);
  varg1 ();

  varg2 (1,2,3,4);
  varg2 (1);

  cslist = {1,2,3,4};
  varg2 (cslist{:});

  justnamenotpos (1, 2);

  out = inout (1,2,3,4);
  __printf_assert__ ("%d ", out{:});

  __printf_assert__ ("%d ", nargin);

  % TODO: Check in caller that return is the same
  % b = varargin{:}

  suby (1,2,3);
end

function varg1 (varargin)
  __printf_assert__ ("%d ", varargin{:});
  __printf_assert__ ("%d ", size (varargin));
  __printf_assert__ ("%d ", nargin);
end

function varg2 (a, varargin)
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", varargin{:});
  __printf_assert__ ("%d ", size (varargin));
  __printf_assert__ ("%d ", nargin);

  varg1 (varargin{:})
  varg1 (2, varargin{:})
end

function justnamenotpos (varargin, a)
  __printf_assert__ ("%d ", a);
  __printf_assert__ ("%d ", varargin);
  __printf_assert__ ("%d ", nargin);
end

function [varargin] = inout (varargin)
  __printf_assert__ ("%d ", nargin);
end

function suby(a,b,c)
  __printf_assert__ ("%d ", nargin);

  if nargin == 3
    suby (1,2);
  elseif nargin == 2
    suby (1);
  elseif nargin == 1
    suby ();
  end
end
