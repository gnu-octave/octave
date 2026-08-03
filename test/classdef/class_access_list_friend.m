classdef class_access_list_friend
  methods (Static)
    function r = call_secret (obj)
      r = secret (obj);
    endfunction
    function r = call_secret_single (obj)
      r = secret_single (obj);
    endfunction
    function obj = set_value (obj, v)
      obj.value = v;
    endfunction
  endmethods
endclassdef
