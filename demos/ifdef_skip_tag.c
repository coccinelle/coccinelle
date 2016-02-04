/* Example provided by: Flavien@lebarbe.net */


int foo(int x) {
    /* {{coccinelle:skip_start}} */
#ifdef PLATFORM_A
    while(func_a()) {
#endif
    /* {{coccinelle:skip_end}} */
#ifdef PLATFORM_B
    while(func_b()) {
#endif
        do_stuff();
    }
}


void main()
{
}
