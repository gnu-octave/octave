function bytecode_index_obj ()

    % Chained indexing with struct
    __printf_assert__ ("%d ", suby1 ().a);
    __printf_assert__ ("%d ", suby1.a); % cmd fn call
    __printf_assert__ ("%d ", suby1 ().b.a);
    __printf_assert__ ("%d ", suby1.b.a);

    % Chained indexing with matrix
    __printf_assert__ ("%d ", suby_mat1 ()(2));

    % Chained indexing with cells
    __printf_assert__ ("%s ", class(suby_cell1 ()(2)));
    __printf_assert__ ("%d ", size(suby_cell1 ()(2)));
    __printf_assert__ ("%d ", suby_cell1 (){1}{3});
    __printf_assert__ ("%d ", suby_cell1 (){1}{3}(1));
    __printf_assert__ ("%d ", suby_cell1 ()(1){1}{2});
    __printf_assert__ ("%d ", suby_cell1{1}{3}); % cmd fn call

    % Dynamic struct field
    s = struct;
    s.qwe = 22;
    s.asd = struct ("qwe", 33);

    __printf_assert__ ("%d ", s.("qwe"));
    __printf_assert__ ("%s ", class (s.("asd").("qwe")));
    __printf_assert__ ("%d ", s.("asd").("qwe"));

    % Subassign dynamic field
    t.qwe = 3;
    t.("asd") = 4;
    __printf_assert__ ("%d ", t.qwe);
    __printf_assert__ ("%d ", t.asd);

    % : and end for eg. foo()(:,end) etc

    % Check that classdef metas can be used to construct a classdef object
    h = @sin;
    o = matlab.lang.MemoizedFunction (h);
    __printf_assert__ ("%s ", class (o))

    % Check proper argument order
    s = [struct struct ; struct struct];
    M = [1:10 ; 11:20];
    s(1,2).a = M;
    __printf_assert__ ("%d ", s(1,2).a(1,2));
end

function s = suby1()
    s = struct;
    s.a = 2;
    b = struct;
    b.a = 3;
    s.b = b;
end

function m = suby_mat1()
    m = [1 2 3];
end

function c = suby_cell1()
    c = {{1 2 3}, 4, 5};
end
