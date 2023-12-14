function bytecode_varargout ()

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

    [a, ~, c] = return_isargout (2);
    __printf_assert__ ("%d ", a);

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

    % Check that 40000 return values wont cause a stack overflow
    ans = 0;
    suby1 (40000); % returns [varargout] => [1 2 3 ... n]
    assert (ans == 1)

    ans = 0;
    suby2 (40000); % returns [a b varargout] => [1 2 3 ... n]
    assert (ans == 1)

    % Check dropping return values
    [a b c] = suby1 (10);
    assert (all ([a b c] == [1 2 3]))
    [a b c] = suby2 (10);
    assert (all ([a b c] == [1 2 3]))

    % Check too few return values
    threw = false;
    try
        [a b c] = suby1 (2);
    catch
        threw = true;
    end
    assert (threw);

    % Bug #65029, stack overflowed when deal returned
    nlay = 20000;
    a = struct ("aa", {1:nlay});
    tmp = {1:nlay};
    [a(1:nlay).aa] = deal (tmp{:});
end

function [a b c] = sub_return_isargout (n)
    b = 0; c = 0;
    a = isargout (n);
end

function varargout = suby1(n)
    for i = 1:n
        varargout{i} = i;
    end
end

function [a b varargout] = suby2(n)
    a = 1; b = 2;
    for i = 1:n-2
        varargout{i} = i + 2;
    end
end

