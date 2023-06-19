% For use by bytecode_disp().
%
% Overload display
%
function display (x)
    if (inputname(1))
        __printf_assert__ ("%s = %d ", inputname(1), x);
    else
        __printf_assert__ ("%d ", x);
    end
end