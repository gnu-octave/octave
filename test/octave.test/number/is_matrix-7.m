implicit_str_to_num_ok = 0;
t1 = ! is_matrix (["test"; "ing"]);
implicit_str_to_num_ok = 1;
t2 = is_matrix (["test"; "ing"]);
t1 && t2
