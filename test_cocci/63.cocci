@@
struct gendisk g;
constant char *string;
@@

- g.major_name = string
+ g.disk_name = string

@@
struct gendisk g;
identifier name;
expression E;
@@

-   sprintf(name + E, ...);
+   sprintf(g.disk_name, ...);
    ...
-   g.major_name = name + E;


@@
@@

-   char name[...];
    ...              WHEN != name

@@
@@

-   char *name;
    ...
      ...
-     name = kmalloc(...);
?     if (!name) { ... }
      ...        WHEN != name
-     kfree(name);
      ...
    ...

@@
struct gendisk g;
extern struct E;
identifier field;
@@

  sprintf(E.field, ...);
  ...
- g.major_name = E.field;
+ strcpy(g.disk_name,E.field);

@@
struct gendisk g;
@@

- if (g.major_name)
+ if (g.part)

@@
struct gendisk g;
@@

- g.major_name
+ g.disk_name
