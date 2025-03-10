int expr;

int main () {
  decltype(expr) expr2;
  decltype((expr)) expr3;
  return expr2 + expr3;
}
