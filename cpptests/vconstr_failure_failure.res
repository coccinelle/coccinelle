struct A {
	int j;
	public:
	virtual  A(){int j;} // warning: constructors cannot be declared ‘virtual’; tolerable with e.g. g++ -fpermissive
	virtual ~A(){int j;} // destructors can be declared 'virtual'
};
int main(){
	int j;
	A a;
}
