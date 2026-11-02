classdef class_bug67512
  properties (Constant)
    Data = class_bug67512_data ();
    Arr = [class_bug67512_data(), class_bug67512_data()];
    S = struct ("f", 1);
    H = struct ("h", class_bug67512_data ());
    C = {class_bug67512_data()};
  endproperties
endclassdef
