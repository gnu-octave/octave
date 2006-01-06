%% Automatically generated from DejaGNU files

%% test/octave.test/prefer/prefer-1.m
%!test
%! m = [3 2];
%! assert(all (m == (3:-1:2)));

%% test/octave.test/prefer/prefer-2.m
%!test
%! m = [3,
%! 2];
%! assert(all (m == (3:-1:2)'));

%% test/octave.test/prefer/prefer-3.m
%!test
%! a = 2;
%! assert([a - 1],1);

%% test/octave.test/prefer/prefer-4.m
%!test
%! m = [3,2];
%! fail("[m ']");

%% test/octave.test/prefer/prefer-5.m
%!assert(all ([3 2] == (3:-1:2)));

%% test/octave.test/prefer/prefer-6.m
%!assert(all ([3, 2] == (3:-1:2)));

%% test/octave.test/prefer/prefer-7.m
%!test
%! m = [3,2];
%! assert(all ([m (1)] == (3:-1:1)));

%% test/octave.test/prefer/prefer-8.m
%!test
%! m = [3,2];
%! assert([m(1)],3);

%% test/octave.test/prefer/prefer-9.m
%!test
%! m = [3,2];
%! assert(all ([m (1)] == (3:-1:1)));

%% test/octave.test/prefer/prefer-10.m
%!test
%! a = 2;
%! assert([a- 1],1);

%% test/octave.test/prefer/prefer-11.m
%!test
%! a = 1;
%! assert(all ([a -1] == (1:-2:-1)));

%% test/octave.test/prefer/prefer-12.m
%!test
%! wsn = warn_str_to_num;
%! warn_str_to_num = 0;
%! assert("d" + 0,100);
%! warn_str_to_num = wsn;

%% test/octave.test/prefer/prefer-13.m
%!test
%! wsn = warn_str_to_num;
%! warn_str_to_num = 1;
%! fail("'d' + 0","warning");
%! warn_str_to_num = wsn;

%% test/octave.test/prefer/prefer-14.m
%!test
%! wir = warn_imag_to_real;
%! warn_imag_to_real = 0;
%! assert(eye (1+i),1);
%! warn_imag_to_real = wir;

%% test/octave.test/prefer/prefer-15.m
%!test
%! wir = warn_imag_to_real;
%! warn_imag_to_real = 1;
%! fail("eye (1+i)","warning");
%! warn_imag_to_real = wir;

%% test/octave.test/prefer/prefer-17.m
%!test
%! wrre = warn_resize_on_range_error;
%! warn_resize_on_range_error = 0;
%! clear a; 
%! a(2) = 1; a(3) = 2; 
%! assert(all (a == [0,1,2]));
%! warn_resize_on_range_error = wrre;

%% test/octave.test/prefer/prefer-18.m
%!test
%! clear a; 
%! a(1) = 1; a(2) = 2;
%! assert(all (a == [1,2]));

%% XXX FIXME XXX How the hell do I test this one in test/assert 
%% test/octave.test/prefer/prefer-19.m
%!#test
%! pid = print_answer_id_name
%! print_answer_id_name = 0;
%! a = 1
%! print_answer_id_name = pid;

%% XXX FIXME XXX How the hell do I test this one in test/assert 
%% test/octave.test/prefer/prefer-20.m
%!#test
%! pid = print_answer_id_name
%! print_answer_id_name = 1;
%! a = 1
%! print_answer_id_name = pid;

%% test/octave.test/prefer/prefer-21.m
%!test
%! ped = print_empty_dimensions;
%! print_empty_dimensions = 0;
%! a = cell (1, 1);
%! b = type -q a;
%! assert(!isempty(findstr(b,"[]")));
%! assert(isempty(findstr(b,"[](0x0)")));
%! print_empty_dimensions = ped;

%% test/octave.test/prefer/prefer-22.m
%!test
%! ped = print_empty_dimensions;
%! print_empty_dimensions = 1;
%! a = cell (1, 1);
%! b = type -q a;
%! assert(!isempty(findstr(b,"[](0x0)")));
%! print_empty_dimensions = ped;

%% test/octave.test/prefer/prefer-23.m
%!assert(all (size (inv ([])) == [0, 0]));

%% test/octave.test/prefer/prefer-24.m
%!assert(all (svd ([]) == zeros (0, 1)));

%% XXX FIXME XXX return_last_computed_value no longer exists!!
%% Remove the next two tests
%% test/octave.test/prefer/prefer-25.m
%% test/octave.test/prefer/prefer-26.m

%% test/octave.test/prefer/prefer-27.m
%!test
%! sp = save_precision;
%! save_precision = 1;
%! x = pi;
%! nm = tmpnam();
%! save("-text",nm,"x");
%! clear x;
%! load(nm);
%! unlink(nm);
%! assert(x,3);
%! save_precision = sp;

%% test/octave.test/prefer/prefer-28.m
%!test
%! sp = save_precision;
%! save_precision = 5;
%! x = pi;
%! nm = tmpnam();
%! save("-text",nm,"x");
%! clear x;
%! load(nm);
%! unlink(nm);
%! assert(x,3.1416);
%! save_precision = sp;

%% XXX FIXME XXX Same problem as above!!!
%% test/octave.test/prefer/prefer-29.m
%!function f ()
%! 1
%!#test
%! sf = silent_functions;
%! silent_functions = 0;
%! f
%! assert(??);
%! silent_functions = sf;

%% XXX FIXME XXX Same problem as above!!!
%% test/octave.test/prefer/prefer-30.m
%!function f ()
%! 1
%!#test
%! sf = silent_functions;
%! silent_functions = 1;
%! f
%! assert(??);
%! silent_functions = sf;

%% test/octave.test/prefer/prefer-32.m
%!test
%! wndz = warn_neg_dim_as_zero;
%! warn_neg_dim_as_zero = 1;
%! fail("eye (-1) == []","warning");
%! warn_neg_dim_as_zero = wndz;

%% test/octave.test/prefer/prefer-33.m
%!test
%! wndz = warn_neg_dim_as_zero;
%! warn_neg_dim_as_zero = 0;
%! assert(all (size (eye (-1)) == [0, 0]));
%! warn_neg_dim_as_zero = wndz;

%% test/octave.test/prefer/prefer-34.m
%!test
%! watv = warn_assign_as_truth_value;
%! warn_assign_as_truth_value = 0;
%! if (x = 1) 1; endif
%! warn_assign_as_truth_value = watv;

%% test/octave.test/prefer/prefer-35.m
%!test
%! watv = warn_assign_as_truth_value;
%! warn_assign_as_truth_value = 1;
%! fail("if (x = 1) 1; endif","warning");
%! warn_assign_as_truth_value = watv;

%% test/octave.test/prefer/prefer-38.m
%!test
%! wdbz = warn_divide_by_zero;
%! warn_divide_by_zero = 0;
%! assert(isinf (1/0));
%! warn_divide_by_zero = wdbz;

%% test/octave.test/prefer/prefer-39.m
%!test
%! wdbz = warn_divide_by_zero;
%! warn_divide_by_zero = 1;
%! a = 1;
%! b = 0;
%! fail("isinf (a/b);","warning")
%! warn_divide_by_zero = wdbz;

