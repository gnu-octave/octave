string_fill_char = setstr (0);
(fnmatch ("a*a", ["aba"; "xxxba"; "aa"]) == [1; 0; 1]
 && fnmatch (["a*a"; "b*b"], "bob")
 && fnmatch ("x[0-5]*", ["x1"; "x6"]) == [1; 0]
 && fnmatch ("x[0-5]*", ["x1"; "x6"; "x001"]) == [1; 0; 1]
 && fnmatch ("x???y", ["xabcy"; "xy"]) == [1; 0])
