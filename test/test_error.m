%% Automatically generated from DejaGNU files

%% test/octave.test/error/error-1.m
%!function g () 
%! error ("foo");
%!function f () 
%! g (); 
%!error <foo> f ();

%% test/octave.test/error/error-2.m
%!function g () 
%! error ("foo\n");
%!function f () 
%! g 
%!error <foo> f ();

%% test/octave.test/error/error-3.m
%!error error ();

%% test/octave.test/error/error-4.m
%!error <foo> error ("foo\n");

%% XXX FIXME XXX Why can't I use %!warning <foo> f;
%% test/octave.test/error/warning-1.m
%!function g ()
%! warning ("foo");
%!function f ()
%! g;
%!test
%! fail("f","warning","foo");

%% test/octave.test/error/warning-2.m
%!test
%! st.identifier = "backtrace";
%! ws = warning ("query","backtrace");
%! warning ("on","backtrace");
%! st.state = "on";
%! assert(warning ("query","backtrace"),st);
%! warning ("off","backtrace");
%! st.state = "off";
%! assert(warning ("query","backtrace"),st);
%! warning (ws.state,"backtrace");

%% XXX FIXME XXX This test no longer makes sense with new warning syntax
%% test/octave.test/error/warning-3.m
%!#warning <foo> warning ("foo", 1);

%% test/octave.test/error/usage-1.m
%!function g () 
%! usage ("foo");
%!function f () 
%! g (); 
%!error <usage:> f ();

%% test/octave.test/error/usage-2.m
%!function g () 
%! usage ("foo");
%!function f () 
%! g 
%!error <usage:> f ();

%% test/octave.test/error/usage-3.m
%!error usage ();

%% test/octave.test/error/usage-4.m
%!error <foo> usage ("foo\n");

