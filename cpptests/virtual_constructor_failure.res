
class A {
	public:
	virtual  A(){} // warning: constructors cannot be declared ‘virtual’; tolerable with e.g. g++ -fpermissive
	virtual ~A(){} // destructors can be declared 'virtual'
};
int main(){
	A a;
}
