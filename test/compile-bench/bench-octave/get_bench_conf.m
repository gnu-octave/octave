function conf = get_bench_conf ()
  conf = {
    % name              argument        O(n^x) dunno  is_script
    {"for_loop_empty", {"n", 206824596}, 1, {}, 0},
    {"for_loop_silly", {"n", 34894840}, 1, {}, 0},
    {"for_loop_binop_1", {"n", 20300088}, 1, {}, 0},
    {"for_loop_binop_2", {"n", 10300088}, 1, {}, 0},
    {"for_loop_binop_2_script", {"n", 10300088}, 1, {}, 1},
    {"for_loop_sinpi", {"n", 12991066}, 1, {}, 0},
    {"for_loop_ifs", {"n", 5874007}, 1, {}, 0},
    {"while_loop_empty", {"n", 24237997}, 1, {}, 0},
    {"do_until_loop_empty", {"n", 27109647}, 1, {}, 0},
    {"for_loop_subfun_1", {"n", 11930390}, 1, {}, 0},
    {"for_loop_call_script_1", {"n", 11930390}, 1, {}, 1},
    {"for_loop_matselfmul", {"rand sq",150}, 3, {}, 0},
    {"for_sum_1", {"rand rowvec", 19267692}, 1, {}, 0},
    {"for_sum_2", {"rand rowvec", 8742659}, 1, {}, 0},
    {"qsort_recursive", {"rand rowvec", 107851}, 1, {}, 0}, % Mostly copies vectors around
    {"qsort_iterative", {"rand rowvec", 344418}, 1, {}, 0},
    {"for_loop_fncall", {"n", 2164885}, 1, {}, 0},
    {"bench_median", {"rand rowvec", 1927}, 1, {}, 0},
    {"bench_cov", {"rand rowvec", 15261}, 1, {}, 0},
    {"str_mod", {"n", 2335290}, 1, {}, 0},
    {"fib", {"n", 31}, 1, {}, 0},
    {"cdef_ctor", {"n", 94964}, 1, {}, 0},
    {"cdef_method1", {"n", 164837}, 1, {}, 0},
  };
end
