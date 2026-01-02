classdef overloaded_permute_class_handle < handle
  properties
    data
  end

  % Overload of the "permute" method
  %
  % The overloaded permute method first does a circshift on the dimorder
  % argument, so
  %
  % >> permute(A, [1, 2, 3, 4])
  %
  % what is ultimately called is "permute(A, [4, 1, 2, 3])"

  methods
    function B = permute (A, dimorder)
      if (numel (A) == 1)
        B = A;
        return;
      end

      % Do a right circshift on dimorder
      dimorder = circshift (dimorder, 1);

      B = builtin ('permute', A, dimorder);
    end
  end
end
