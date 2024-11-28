struct A {
	int i;
	public:
	virtual  A(){int i;} // warning: constructors cannot be declared ‘virtual’; tolerable with e.g. g++ -fpermissive
	virtual ~A(){int i;} // destructors can be declared 'virtual'
};
int main(){
	int i;
	A a;
}
