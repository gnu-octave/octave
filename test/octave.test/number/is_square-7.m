implicit_str_to_num_ok = 0;
t1 = ! is_square (["test"; "ing"; "1"; "2"]);
implicit_str_to_num_ok = 1;
t2 = (is_square (["test"; "ing"; "1"; "2"]) == 4);
t1 && t2
