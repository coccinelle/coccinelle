# spatch --c++
@@ @@
-	new A[3];
@@ @@
-	B*b=new B    {1,2};
@@ @@
-	new C    (3);
@@ @@
-	C*c=new C[2] {1,2};
@@ @@
-	new C*[2];
