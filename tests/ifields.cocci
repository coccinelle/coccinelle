@ object @
typedef IFace;
typedef IFaceVtbl;
type Tobj;
field list[nilla] F;
@@
  typedef struct tag_obj {
      F
-     const IFaceVtbl *lpVtbl;
+     IFace IFace_iface;
      ...
  } Tobj;


@s@
identifier obj;
identifier vtbl;
initializer list[object.nilla] E;
@@
  static struct IFaceImpl obj = {
      E,
-     &vtbl,
+     { &vtbl, 4, },
      ...,
  };
