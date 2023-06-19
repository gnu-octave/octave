function bytecode_inputname (arg1, arg2)
    __printf_assert__ ("%s ", inputname (1, 0));
    __printf_assert__ ("%s ", inputname (1, 1));
    __printf_assert__ ("%s ", inputname (2, 0));
    __printf_assert__ ("%s ", inputname (2, 1));

    a = 2;
    b = 3;
    suby (a, b);
    suby (a + 1, b * 3);

    % inputname from non-compiled function
    inputname_args (a, b);
    inputname_args (a + 1, b * 3);
end

function suby (arg1, arg2)
    __printf_assert__ ("%s ", inputname (1, 0));
    __printf_assert__ ("%s ", inputname (1, 1));
    __printf_assert__ ("%s ", inputname (2, 0));
    __printf_assert__ ("%s ", inputname (2, 1));

    aa = 22;
    bb = 33;
    % inputname from non-compiled function
    inputname_args (aa, bb);
    inputname_args (aa + 1, bb * 3);
end