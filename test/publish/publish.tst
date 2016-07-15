## publish

%!testif HAVE_X_WINDOWS
%! cases = dir ("test_script*.m");
%! cases = strsplit (strrep ([cases.name], ".m", ".m\n"));
%! for i = 1:length(cases)-1
%!   publish (cases{i});
%! endfor
%! confirm_recursive_rmdir (false, "local");
%! rmdir ("html", "s");

## grabcode

%!testif HAVE_X_WINDOWS
%! publish ("test_script.m");
%! str1 = fileread ("test_script.m");
%! str2 = grabcode ("html/test_script.html");
%! confirm_recursive_rmdir (false, "local");
%! rmdir ("html", "s");
%! # Canonicalize strings
%! str1 = strjoin (deblank (strsplit (str1, "\n")), "\n");
%! str2 = strjoin (deblank (strsplit (str2, "\n")), "\n");
%! assert (hash ("md5", str1), hash ("md5", str2));