  typedef struct tag_obj {
    int x;
    int y;
    const IFaceVtbl *lpVtbl;
    int a;
  } Tobj;


  static struct IFaceImpl obj = {
    1, 2,
      &x,
      3
  };

  static struct IFaceImpl obj1 = {
    1, 2, 6,
      &x,
      3
  };
