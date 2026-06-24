% Nested functions frame stored in handles would prevent parent frame
% from being released causing memory leak.

%!test <*64620>
%! % Counts amount of alive cdef_counts_self handle objects
%! global cdef_alive_objects_cntr
%! cdef_alive_objects_cntr = 0;
%! bug64620_1;
%! assert (cdef_alive_objects_cntr == 0);
%!
%! clear -global cdef_alive_objects_cntr

%!test <*64620>
%! bug64620_2;  % Asserts inside itself
%! clear -global cdef_alive_objects_cntr
