charset = setstr (0:127);

result = ones (1, 128);

all (isascii (charset) == result)
