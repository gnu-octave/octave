implicit_str_to_num_ok = 0;
t1 = ! issquare (["test"; "ing"; "1"; "2"]);
implicit_str_to_num_ok = 1;
t2 = ! issquare (["test"; "ing"; "1"; "2"]);
t1 && t2
