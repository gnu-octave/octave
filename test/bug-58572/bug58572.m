classdef bug58572
  methods
    function rslt = use_num (obj)
      rslt = cellfun (@isnumeric, {'a_name'});
    endfunction
    function rslt = isnumeric (obj)
      if (! isa (obj, 'bug58572'))
        error ('bug58572.isnumeric called without object!');
      endif
      rslt = true;
    endfunction
  endmethods
endclassdef
