@a@
identifier x;
@@

foo(x);

@script:python b@
x << a.x;
y;
z;
@@

print y
coccinelle.y = x
coccinelle.z = "something"
print y

@c@
identifier b.y;
identifier b.z;
identifier a.x;
@@

- bar();
+ matched_bar(y,z,x);
