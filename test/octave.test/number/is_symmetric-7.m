implicit_str_to_num_ok = 0;
t1 = ! is_symmetric (["te"; "et"]);
implicit_str_to_num_ok = 1;
t2 = (is_symmetric (["te"; "et"]) == 2);
t1 && t2
