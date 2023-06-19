function bytecode_varargout ()
        disp ("foobar")
    % Just check this works
    [a b] = {7, 8}{:};
    __printf_assert__ ("%d %d ", a, b);

    % n amount of vargouts from suby1(n)
    a = suby1 (1);
    __printf_assert__ ("%d ", a);
    [a b] = suby1 (2);
    __printf_assert__ ("%d %d ", a, b);

    % Test that ignored outputs are set properly
    % when calling interpreted functions.
    %
    % return_isargout (n) returns isargout (n) in its
    % first output.
    [a b c] = return_isargout (2);
    __printf_assert__ ("%d ", a);

    [a b c] = return_isargout (4);
    __printf_assert__ ("%d ", a);
 disp ("foobar2")
    [a, ~, c] = return_isargout (2);
    __printf_assert__ ("%d ", a);
 disp ("foobar22")
    [a, ~, ~] = return_isargout (2);
    __printf_assert__ ("%d ", a);
    [a, ~, ~] = return_isargout (1);
    __printf_assert__ ("%d ", a);
    [a, ~, ~] = return_isargout (3);
    __printf_assert__ ("%d ", a);

    [~, ~, ~] = return_isargout (3);

    % Do the same for a vm function
    [a b c] = sub_return_isargout (2);
    __printf_assert__ ("%d ", a);

    [a b c] = sub_return_isargout (4);
    __printf_assert__ ("%d ", a);

    [a, ~, c] = sub_return_isargout (2);
    __printf_assert__ ("%d ", a);

    [a, ~, ~] = sub_return_isargout (2);
    __printf_assert__ ("%d ", a);
    [a, ~, ~] = sub_return_isargout (1);
    __printf_assert__ ("%d ", a);
    [a, ~, ~] = sub_return_isargout (3);
    __printf_assert__ ("%d ", a);

    [~, ~, ~] = sub_return_isargout (3);
end

function [a b c ] = sub_return_isargout (n)
    b = 0; c = 0;
    a = isargout (n);
end

function varargout = suby1(n)
    for i = 1:n
        varargout{i} = i;
    end
end