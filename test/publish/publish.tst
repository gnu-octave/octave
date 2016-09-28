## publish

%!test
%! visibility = get (0, "defaultfigurevisible");
%! unwind_protect
%!   set (0, "defaultfigurevisible", "off");
%!   cases = dir ("test_script*.m");
%!   cases = strsplit (strrep ([cases.name], ".m", ".m\n"));
%!   for i = 1:length(cases)-1
%!     publish (cases{i});
%!   endfor
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir ("html", "s");
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%! end_unwind_protect

## grabcode

%!test
%! visibility = get (0, "defaultfigurevisible");
%! unwind_protect
%!   set (0, "defaultfigurevisible", "off");
%!   publish ("test_script.m");
%!   str1 = fileread ("test_script.m");
%!   str2 = grabcode ("html/test_script.html");
%!   confirm_recursive_rmdir (false, "local");
%!   rmdir ("html", "s");
%!   # Canonicalize strings
%!   str1 = strjoin (deblank (strsplit (str1, "\n")), "\n");
%!   str2 = strjoin (deblank (strsplit (str2, "\n")), "\n");
%!   assert (hash ("md5", str1), hash ("md5", str2));
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", visibility);
%! end_unwind_protect
