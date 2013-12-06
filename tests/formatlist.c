int main () {
  foo("xyz %d abc");
  foo("xyz %d %d abc");
  foo("mno %d %d abc");
  foo("mno %d abc %d %d abc %d");
  foo("xyz %d abc %d %d abc %d");
  foo("xyz %d abc %d %d abc %d mno");
  foo("xyz %d abc %d %d abc %d %d abc %d");
  foo("xyz %d abc %d %d abc");
  foo("xyz abc");
}
