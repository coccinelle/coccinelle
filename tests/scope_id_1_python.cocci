@initialize:python@
@@
offset = 0

def split(s):
    global offset
    if s == "":
        return ""
    if offset == 0:
        offset = s.index("linux-")
    return s[offset:]

@r@
int e;
position p;
@@

e@p

@script:python@
e << r.e;
p << r.p;
@@

print("{}: {}: {}: {}".format(e,
    " ".join((split(filename) for filename in cocci.files())),
    split(p[0].file), p[0].line))
