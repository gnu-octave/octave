new_var_name = matlab.lang.makeUniqueStrings ('somevar', who ());
eval (sprintf ("global %s", new_var_name))
