[a, b, c] = sscanf ("1.2 3 foo", "%f%d%s", "C");
[v1, c1, m1] = sscanf ("1 2 3 4 5 6", "%d")
[v2, c2, m2] = sscanf ("1 2 bar 3 4 5 6", "%d")

(a == 1.2 && b == 3 && c == "foo"
 && v1 == [1; 2; 3; 4; 5; 6] && c1 == 6 && isstr (m1)
 && v2 == [1; 2] && c2 == 2 && isstr (m2))
