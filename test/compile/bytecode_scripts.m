function bytecode_scripts
    % Each variable is named after the corrensponding script that messes with them.
    % When eval() is used, it is to not introduce the variable in the static precompiled
    % stack frame, to test the dynamic stack frame.

    b1 = 2;
    __printf_assert__ ("%d ", exist ("a1"));
    script1 (); % defines 'a1', 'b1', 'c1', 'd1'

    __printf_assert__ ("%d ", eval ("a1;"));
    __printf_assert__ ("%d ", b1);
    __printf_assert__ ("%d ", eval ("c1;"));
    __printf_assert__ ("%d ", d1);

    try
        script2 (); % defines 'a2 = 3' and then errors
    catch
        __printf_assert__ ("%d ", a2);
    end

    script3 ();

    % Inline function defined in script1
    assert (inlinefn2 (234) == 235)
end
