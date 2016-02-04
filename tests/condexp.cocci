@@
identifier displayname;
@@
(
-XDisplayName(NULL)
+getenv("DISPLAY")
|
-XDisplayName(displayname)
+displayname ? displayname : getenv("DISPLAY")
)
