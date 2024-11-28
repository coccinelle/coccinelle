#spatch --c++
// neither patch not source work yet
@@
@@

- template <class T> struct ts  { ... };
- template <> struct ts<int>  {
- 	...
- };

- template <> struct ts<int>::ns<int>  { ... };
- template <> int ts<int>::h<int>() { ... };

int main()
{
- ...
}
