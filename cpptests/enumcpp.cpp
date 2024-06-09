int main()
{
  enum struct l1 {A, B};
  enum class  l2 {A2, B2};
  enum struct l3:int {A3,B3};
  enum class  l4:int {A4,B4};

  enum l11 {A11, B11};
  enum l12 {A12, B12};
  enum l13:int {A13,B13};
  enum l14:int {A14,B14};

  enum {A21, B21} l21;
  enum {A22, B22} l22;
  enum :int {A33,B33} l33;
  enum :int {A32,B32} l32;

  enum struct zl1 {};
  enum class  zl2 {};
  enum struct zl3:int {};
  enum class  zl4:int {};

  enum zl11 {};
  enum zl12 {};
  enum zl13:int {};
  enum zl14:int {};

  enum {} zl21;
  enum {} zl22;
  enum :int {} zl33;
  enum :int {} zl32;
}
