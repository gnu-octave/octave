charset = setstr (0:127);

result = zeros (1, 128);

result ((toascii("a"):toascii("z"))+1) = 1;

all (islower (charset) == result)
