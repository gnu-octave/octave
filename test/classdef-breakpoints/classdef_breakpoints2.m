classdef classdef_breakpoints2 < handle
  properties (Dependent)
    m_prop;
  endproperties
  properties (SetAccess=private)
    m_prop_value = [];
  endproperties
  methods
    function this = classdef_breakpoints2 (prop)
      this.m_prop = prop;
    endfunction
    function val = get.m_prop (this)
      val = this.m_prop_value;
    endfunction
    function set.m_prop (this, val)
      this.m_prop_value = val;
    endfunction
  endmethods
endclassdef
