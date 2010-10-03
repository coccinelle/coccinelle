@@ @@
void foo(char *text) {
-   strcat(buf->data, text);
+  strcat_safe(buf->data, buf->len, text);
}
