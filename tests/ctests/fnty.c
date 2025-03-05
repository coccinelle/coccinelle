struct allfns {
  int (*x) (int);
  int (*y) (int);
};

struct somefns {
  int (*x) (int);
  int y;
};

struct nofns {
  int x;
  int y;
};
