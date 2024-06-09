// https://en.cppreference.com/w/cpp/language/constructor

class X
{
    int aa, b, i, j;
public:
    const int& r;
    X(int i)
      : r(aa) // initializes X::r to refer to X::a
      , b{i} // initializes X::b to the value of the parameter i
      , i(i) // initializes X::i to the value of the parameter i
      , j(this->i) // initializes X::j to the value of X::i
    {}
};
