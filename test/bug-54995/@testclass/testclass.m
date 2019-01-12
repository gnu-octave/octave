function obj = testclass ()
  obj = struct ("x", eye (4));
  obj = class(obj, "testclass");
endfunction
