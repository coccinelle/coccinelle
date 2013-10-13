int main () {
  one(argument1(nested, argument), argument2(nested, argument), foo(),
      argument3(nested, argument));
  one(argument1(nested, argument), argument2(nested, argument), foo(), a(b));
  one(argument1(nested, argument), argument2(nested, argument), foo());
}
