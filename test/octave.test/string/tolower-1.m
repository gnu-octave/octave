charset = setstr (0:127);

result = charset;

result ((toascii("A"):toascii("Z"))+1) \
    = result ((toascii("a"):toascii("z"))+1);

all (tolower (charset) == result)
