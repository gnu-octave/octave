charset = setstr (0:127);

result = zeros (1, 128);

result ((toascii("0"):toascii("9"))+1) = 1;

all (isdigit (charset) == result)
