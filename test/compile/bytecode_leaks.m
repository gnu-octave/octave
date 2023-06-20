#
# We are looking for leaks of octave_value:s by checking the refrence counts
# in different code paths of the calling convention.
#
function [a b] = bytecode_leaks (c, d)
  cc = c;
  b = d;

  e = 1+1;
  refs_e = __ref_count (e);
  suby1(e);
  assert (refs_e, __ref_count (e))

  aa = suby2(e);
  assert (refs_e, __ref_count (e))

  % Test special code path for assigning argument to return value
  aa = suby3(e);
  assert (refs_e + 1, __ref_count (e))
  aa = 0;
  assert (refs_e, __ref_count (e))

  % varargin
  suby4 (e,e,e,e);
  assert (refs_e, __ref_count (e))
  suby5 (e,e,e,e);
  assert (refs_e, __ref_count (e))

  % varargout
  suby6(e);
  assert (refs_e, __ref_count (e))
  [tmp1, tmp2, tmp3, tmp4, tmp5] = suby6(e);
  tmp1 = 0; tmp2 = 0; tmp3 = 0; tmp4 = 0; tmp5 = 0;
  assert (refs_e, __ref_count (e))

  suby7(e);
  assert (refs_e, __ref_count (e))
  [tmp1, tmp2, tmp3, tmp4, tmp5] = suby6(e);
  tmp1 = 0; tmp2 = 0; tmp3 = 0; tmp4 = 0; tmp5 = 0;
  assert (refs_e, __ref_count (e))

  % Call non-vm function
  sin (e);
  assert (refs_e, __ref_count (e))

  % Index matrix
  m = [1 2 3 4];
  m (e);
  assert (refs_e, __ref_count (e))

  % Ops
  tmp1 = -e + e * e - e / e ^ e;
  assert (refs_e, __ref_count (e))

  % Dynamic matrix
  m = [1 2 3 e; e 4 5 6];
  m = 0;
  assert (refs_e, __ref_count (e))

  % Cell
  m = {1,2,3, e; 4, 5, e, 6};
  m = 0;
  assert (refs_e, __ref_count (e))

  % "command call" with disp
  disp ("The disp of e and pi underneath is on purpose. There should be a 'e = 2' and 'ans = 3.14...'")
  e % Should print "e = 2"
  assert (refs_e + 1, __ref_count (e)) % in ans
  ans = 0;
  assert (refs_e, __ref_count (e))

  % This will be a function call and should print "ans = 3.14..."
  pi

  % no disp
  e;
  assert (refs_e + 1, __ref_count (e)) % in ans
  ans = 0;
  assert (refs_e, __ref_count (e))


  % Too many or few args
  try
    suby1 (e,e,e);
  catch
  end
  assert (refs_e, __ref_count (e))

  try
    subsuby5 (e);
  catch
  end
  assert (refs_e, __ref_count (e))

  try
    m = [];
    m(e)
  catch
  end
  assert (refs_e, __ref_count (e))

  try
    m = [];
    m(e) = 123;
  catch
  end
  assert (refs_e, __ref_count (e))
  
  % eval dynamic stack
  suby8 (e);
  assert (refs_e, __ref_count (e))
end

function suby1 (a)
  aa = 1 + a;
  bb = a;
end

function aa = suby2 (a)
  aa = 1 + a;
  bb = a;
end

function a = suby3(a)
end

function b = suby4(varargin)
  b = 3 + varargin{1};
  c = varargin{2};
end

function b = suby5(a, b, varargin)
  b = 3 + varargin{1};
  c = varargin{2};
  d = a;
end

function varargout = suby6(a)
  varargout{1} = 3;
  varargout{2} = a;
  varargout{3} = 1;
  varargout{4} = 1;
  varargout{5} = a;
  cc = a;
end

function [aa bb varargout] = suby7(a)
  varargout{1} = 3;
  varargout{2} = a;
  varargout{3} = 1;
  aa = 1;
  bb = a;
  cc = a;
end

function suby8(a)
  eval ("g = a;");
end