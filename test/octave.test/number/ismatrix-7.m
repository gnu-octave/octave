implicit_str_to_num_ok = 0;
t1 = ! ismatrix (["test"; "ing"]);
implicit_str_to_num_ok = 1;
t2 = ismatrix (["test"; "ing"]);
t1 && t2
