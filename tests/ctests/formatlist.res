int main () {
  foo("xyz %d abc");
  foo("blah2");
  foo("mno %d %d abc");
  foo("mno %d abc %d %d abc %d");
  foo("blah");
  foo("blah");
  foo("xyz %d abc %d %d abc %d %d abc %d");
  foo("xyz %d abc %d %d abc");
  foo("xyz abc");
}
