charset = setstr (0:127);

result = zeros (1, 128);

result ((toascii("A"):toascii("Z"))+1) = 1;

all (isupper (charset) == result)
