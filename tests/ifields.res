  typedef struct tag_obj {
    int x;
    int y;
    IFace IFace_iface;
    int a;
  } Tobj;


  static struct IFaceImpl obj = {
    1, 2,
      {
        &x, 4,
      },
      3
  };

  static struct IFaceImpl obj1 = {
    1, 2, 6,
      &x,
      3
  };
