charset = setstr (0:127);

result = zeros (1, 128);

result ((toascii("A"):toascii("Z"))+1) = 1;
result ((toascii("0"):toascii("9"))+1) = 1;
result ((toascii("a"):toascii("z"))+1) = 1;

all (isalnum (charset) == result)
