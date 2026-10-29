classdef ImmutableHandle < handle
  properties (SetAccess = 'immutable')
    value = 7;
    member;
    members;
  endproperties
  methods
    function obj = ImmutableHandle (value, previous, use_helper)
      obj.member = HandleNames ();
      obj.members = [HandleNames(), HandleNames()];
      if (nargin > 2 && use_helper)
        obj.change ();
      endif
      if (nargin > 0)
        obj.value = value;
      endif
      if (nargin > 1)
        previous.value = 0;
      endif
    endfunction
    function change (obj)
      obj.value = 0;
    endfunction
  endmethods
endclassdef
