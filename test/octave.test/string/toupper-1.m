charset = setstr (0:127);

result = charset;

result ((toascii("a"):toascii("z"))+1) \
    = result ((toascii("A"):toascii("Z"))+1);

all (toupper (charset) == result)
