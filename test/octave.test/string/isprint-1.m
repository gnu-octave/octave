charset = setstr (0:127);

result = zeros (1, 128);

result (33:127) = 1;

all (isprint (charset) == result)
