charset = setstr (0:127);

result = zeros (1, 128);

result ((toascii("A"):toascii("F"))+1) = 1;
result ((toascii("0"):toascii("9"))+1) = 1;
result ((toascii("a"):toascii("f"))+1) = 1;

all (isxdigit (charset) == result)
