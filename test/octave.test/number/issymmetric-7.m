implicit_str_to_num_ok = 0;
t1 = ! issymmetric (["te"; "et"]);
implicit_str_to_num_ok = 1;
t2 = ! issymmetric (["te"; "et"]);
t1 && t2
