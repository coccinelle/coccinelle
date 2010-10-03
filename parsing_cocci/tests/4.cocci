@@
struct video_device v;
local function fn;
@@

  v.mmap = fn;

@@
local function fn1;
identifier dev, adr, size;
fresh identifier vma;
@@

  fn(
+    struct vm_area_struct *vma,
     struct video_device *dev, const char *adr, unsigned long size) {
    <***
    fn1(
+       vma,
        ...)
    ***>
  }

  fn1(
+     struct vm_area_struct *vma,
      ...) {
    <***
    remap_page_range(
+                    vma,
                     ...)
    ***>
  }

