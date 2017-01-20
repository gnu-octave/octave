## test for publish

%!test
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (ispc () || ! __have_feature__ ("OSMESA"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support OSMESA or gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   scripts = dir ("test_script*.m");
%!   for fname = {scripts.name}
%!     publish (fname{1});
%!   endfor
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir ("html", "s");
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

## test for grabcode

%!test
%! visibility = get (0, "defaultfigurevisible");
%! toolkit = graphics_toolkit ();
%! unwind_protect
%!   if (ispc () || ! __have_feature__ ("OSMESA"))
%!     try
%!       graphics_toolkit ("gnuplot");
%!     catch
%!       ## The system doesn't support OSMESA or gnuplot for drawing hidden
%!       ## figures.  Just return and have test marked as passing.
%!       return;
%!     end_try_catch
%!   endif
%!   set (0, "defaultfigurevisible", "off");
%!
%!   publish ("test_script.m");
%!   str1 = fileread ("test_script.m");
%!   str2 = grabcode ("html/test_script.html");
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir ("html", "s");
%!   ## Canonicalize strings
%!   str1 = strjoin (deblank (strsplit (str1, "\n")), "\n");
%!   str2 = strjoin (deblank (strsplit (str2, "\n")), "\n");
%!   assert (hash ("md5", str1), hash ("md5", str2));
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect
