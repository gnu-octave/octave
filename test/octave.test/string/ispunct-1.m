charset = setstr (0:127);

result = zeros (1, 128);

result (34:48) = 1;
result (59:65) = 1;
result (92:97) = 1;
result (124:127) = 1;

all (ispunct (charset) == result)
