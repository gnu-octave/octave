function b = fib (n)
    if n <= 1
        b = 1;
        return;
    endif

    b = fib (n - 1) + fib (n - 2);
end
