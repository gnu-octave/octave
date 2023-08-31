% Nested functions frame stored in handles would prevent parent frame
% from being released causing memory leak.

%!test <*64620>
%! global cdef_alive_objects_cntr % Counts amount of alive cdef_counts_self handle objects
%! cdef_alive_objects_cntr = 0;
%! bug_64620_1;
%! assert (cdef_alive_objects_cntr == 0)
%!
%! clear -global cdef_alive_objects_cntr

%!test <*64620>
%! bug_64620_2; % Asserts inside itself
%! clear -global cdef_alive_objects_cntr
