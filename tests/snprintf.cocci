@rp@
identifier show,buf;
@@

ssize_t show(char *buf)
{
        <...
(
        return
-               snprintf
+               sprintf
                        (buf,
                        ...);
|
        return
-               snprintf
+               scnprintf
                        (...);
)
        ...>
}
