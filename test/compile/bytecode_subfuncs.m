function bytecode_subfuncs (h_to_anon_fn)
  a = foo ();
  __printf_assert__ ("%.17g ", a);
  b = bar ();
  __printf_assert__ ("%.17g ", b);
  c = baz ();
  __printf_assert__ ("%.17g ", c);

  meh ();

  a = para_ret_same_name (11);
  __printf_assert__ ("%.17g ", a);

  % Test default args
  a = defaultarg ();
  __printf_assert__ ("%.17g ", a);

  a = defaultarg (10);
  __printf_assert__ ("%.17g ", a);

  a = defaultarg2 ();
  __printf_assert__ ("%.17g ", a);

  a = defaultarg2 (11);
  __printf_assert__ ("%.17g ", a);

  a = defaultarg2 (11, 12, 13, 14);
  __printf_assert__ ("%.17g ", a);

  % Magic colon
  a = defaultarg2 (11, :, 13, 14);
  __printf_assert__ ("%.17g ", a);

  % Functions handles
  h = @max;
  __printf_assert__ ("%d ", h ([1 3]));
  __printf_assert__ ("%d ", just_call_handle_with_arg (h, [1 3]));
  __printf_assert__ ("%d ", just_call_handle_with_arg_bytecode (h, [1 3]));

  h = @foo;
  __printf_assert__ ("%d ", h ());
  __printf_assert__ ("%d ", just_call_handle_with_arg (h));
  __printf_assert__ ("%d ", just_call_handle_with_arg_bytecode (h));

  % Call an anonymous function from the tst-file
  h_to_anon_fn ();

  % Many args and returns
  [a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32] = ret32 ();
  __printf_assert__ ("%d ", a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
  __printf_assert__ ("%d ", ret32 ()); % nargout = 1
  ret32 (); % nargout = 0
  [args32{1:32}] = ret32 ();
  __printf_assert__ ("%d ", args32{:});

  [a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32] = ret32take32 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);
  __printf_assert__ ("%d ", a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);

  take32 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);

  [a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, ...
   a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63, a64] = ...
      takeXp32retXp32 (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, ...
                       33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64);
  __printf_assert__ ("%d ", a01, a18, a59, a64);
end

function [a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32, varargout] = takeXp32retXp32 (b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32, varargin)
  for i = 1:32
    eval (sprintf ("a%02.f = b%02.f;", i, i));
  end
  varargout = varargin;
end

function take32 (b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32)
  __printf_assert__ ("take32:");
  __printf_assert__ ("%d ", b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32);
end

function [a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32] = ret32 ()
  __printf_assert__ ("ret32:");
  for i = 1:32
    eval (sprintf ("a%02.f = %f;", i, i));
  end
end

function [a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32] = ret32take32 (b01, b02, b03, b04, b05, b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31, b32)
  for i = 1:32
    eval (sprintf ("a%02.f = b%02.f;", i, i));
  end
end

function out = just_call_handle_with_arg_bytecode (h, varargin)
  out = h (varargin{:});
end

function out = foo ()
  out = 2;
end

function out = bar ()
  out = foo ();
end

function out = baz ()
  out = bar ();
end

function meh()
end

function i = para_ret_same_name (i)
end

function a = defaultarg (b = 30)
  a = b;
end

function a = defaultarg2 (a = 30, b = max (4, 5), c = [], d = [1 2])
  __printf_assert__ ("%.17g ", a);
  __printf_assert__ ("%.17g ", b);
  __printf_assert__ ("%.17g ", c);
  __printf_assert__ ("%d ", size(c));
  __printf_assert__ ("%s ", class(c));
  __printf_assert__ ("%.17g ", d);
  __printf_assert__ ("%d ", size(d));
  __printf_assert__ ("%s ", class(d));
end
