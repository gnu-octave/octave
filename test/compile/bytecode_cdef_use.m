function bytecode_cdef_use ()
  % The classdef handle class cdef_foo prints its ctor argument
  % in the dtor with __printf_assert__ aswell asd
  % adds one to the globals cdef_foo_ctor_cnt and cdef_foo_dtor_cnt
  % in the ctor and dtor.

  h1 = cdef_foo("f1");
  __printf_assert__ (". ");
  h1 = 2; % Triggers dtor
  __printf_assert__ (". ");

  h4 = make_obj ("f2") + make_obj ("f3"); % Two dtors will trigger here
  __printf_assert__ (". ");
  __printf_assert__ ("%s %d ", h4.msg, h4.val);

  __printf_assert__ (". ");
  make_obj ("f4");          % Saved in ans
  __printf_assert__ (". "); % Wont trigger dtor since no defined return value
  print_arg_ret_one ("call1"); % ans' dtor executed after rhs eval
  __printf_assert__ (". ");

  % Test calling function in packages. Not really a classdef
  u = matlab.lang.makeUniqueStrings ({"a","a"});
  __printf_assert__ ("%s %s ", u{1}, u{2});

  % Check dtor call order
  a = make_obj ("f5");
  b = make_obj ("f6");
  c = make_obj ("f7");

  suby (); % Same tests in a subfunction
end

function suby ()
  h1 = cdef_foo("f8");
  __printf_assert__ (". ");
  h1 = 2; % Triggers dtor
  __printf_assert__ (". ");

  h4 = make_obj ("f9") + make_obj ("f10"); % Two dtors will trigger here
  __printf_assert__ (". ");
  __printf_assert__ ("%s %d ", h4.msg, h4.val);

  __printf_assert__ (". ");
  make_obj ("f11");          % Saved in ans
  __printf_assert__ (". "); % Wont trigger dtor since no defined return value
  print_arg_ret_one ("call1"); % ans' dtor executed after rhs eval
  __printf_assert__ (". ");

  % Check that the classdef object is called
  m = containers.Map;
  m("qwe") = 2;
  __printf_assert__ ("%d ", m("qwe"));
  __printf_assert__ ("%d ", m("qwe")++); % Test ++-- on objects
  __printf_assert__ ("%d ", m("qwe"));
  __printf_assert__ ("%d ", ++m("qwe"));
  __printf_assert__ ("%d ", m("qwe"));
  __printf_assert__ ("%d ", m("qwe")--);
  __printf_assert__ ("%d ", m("qwe"));
  __printf_assert__ ("%d ", --m("qwe"));
  __printf_assert__ ("%d ", m("qwe"));
  % Different op code than cmd form call
  m = containers.Map();
  m("qwe") = 3;
  __printf_assert__ ("%d ", m("qwe"));

  __printf_assert__ (". ");
  % Check dtor call order
  a = make_obj ("f12");
  b = make_obj ("f13");
  c = make_obj ("f14");
end


function h = make_obj (msg)
    h = cdef_foo (msg);
end

function a = print_arg_ret_one (msg)
  __printf_assert__ (msg);
  a = 1;
end