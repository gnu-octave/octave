charset = setstr (0:127);

result = zeros (1, 128);

result (34:127) = 1;

all (isgraph (charset) == result)
