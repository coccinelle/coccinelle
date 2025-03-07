template <class T> struct ts  {int f(){return 1;}};
template <> struct ts<int>  {
		template <class T> struct ns { int f(){return 2;} };
		template <class T> int h(){ return 3; }
};

template <> struct ts<int>::ns<int>  { int g(){return 0;}; }; // nested and templated struct
template <> int ts<int>::h<int>() { return 0; }; // nested and templated function

int main ()
{
	struct ts<int>::ns<int> v;
	struct ts<int> w;
	return v.g() + w.h<int>();
}
