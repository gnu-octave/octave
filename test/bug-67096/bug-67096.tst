## Test nargout in classdef methods when the method call has no "()" and is
## followed by another indexing operation

%!test <*67096>
%! cb = class_bug67096;
%! [a, b] = cb.get_struct.data;
%! assert( [a, b], [1, 2]);

%!test <*67096>
%! cb = class_bug67096;
%! [a, b] = cb.get_cell{1:2};
%! assert( [a, b], [1, 2]);
