charset = setstr (0:127);

result = 0:127;

all (toascii (charset) == result)
