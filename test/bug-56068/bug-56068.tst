%!test <*56068>
%! res = textscan (evalc ("bug_56068"), "%s", "delimiter", "\n");
%! f_cell = textscan (fileread ("bug_56068.m"), "%s", "delimiter", "\n");
%! f_echo = cellfun (@(x) sprintf ([PS4 "%s"], x), ...
%!                   f_cell{1}([2,3,4,5,8,9,10,11]), ...
%!                   "uniformoutput", false);
%!
%! assert (numel (res{1}), 15);
%! assert (all (strcmp (res{1}([2,3,5,7,10,11,13,15]), f_echo)));
