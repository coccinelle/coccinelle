int x;

template<>
class TestSpMM_OfAnyTypeSquare: public SquareTestMatrix<std::tuple_element_t<0,TV>,std::tuple_element_t<2,TV>>, public ::testing::Test {
 public:
  using NT = typename std::tuple_element_t<0,TV>;
  using IT = typename std::tuple_element_t<2,TV>;
  using Parms = typename std::tuple_element_t<2,TV>;
  using MtxP = SquareTestMatrix <NT,IT>;
  int x[2];
};
