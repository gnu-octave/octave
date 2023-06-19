function bytecode_ans ()
    max (1, 2);
    __printf_assert__ ("%d ", ans);
    1 + 1 + 3;
    __printf_assert__ ("%d ", ans);
    !false;
    __printf_assert__ ("%d ", ans);
    true;
    __printf_assert__ ("%d ", ans);

    c = 13; % Not written to ans
    __printf_assert__ ("%d ", ans);
end