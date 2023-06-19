function [a b c] = return_isargout (n)
    b = 0;
    c = 0;
    a = isargout (n);
end