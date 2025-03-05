struct s {
    int f:4;
};

void
g(struct s *p)
{
    h(p->f);
}
