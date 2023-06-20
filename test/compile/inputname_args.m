function inputname_args (arg1, arg2)
    __printf_assert__ ("%s ", inputname (1, 0));
    __printf_assert__ ("%s ", inputname (1, 1));
    __printf_assert__ ("%s ", inputname (2, 0));
    __printf_assert__ ("%s ", inputname (2, 1));
end
