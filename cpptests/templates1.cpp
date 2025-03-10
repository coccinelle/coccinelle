template <typename T1, class T2=int, const int I1=4, T2 I2=1, const T2 I3=I1+I2>
int f(const T1 v1, const T2 v2)
{
	return I1+I2+I3+v1+v2;
}
int main()
{
	return f<int,int,4,1>(1,1) + f<int,int,-4,-1>(1,2) + f<int,int>(1,2) + f<int>(1,2) - 31;
}
