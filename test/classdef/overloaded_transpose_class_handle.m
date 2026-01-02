classdef overloaded_transpose_class_handle < handle
  properties
    data
  end

  methods
    function B = transpose (A)
      n = numel (A);

      for i = 1:n
        A(i).data = A(i).data.';
      end

      B = A;
    end
  end
end
