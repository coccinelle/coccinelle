void main()
{
    buf = alloca(3
    #ifdef PLATFORM_A
                    +5
    #endif
    #ifdef PLATFORM_B
                    +2
    #endif
            );

}
