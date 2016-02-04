int main() {
    buf = alloca(3
    #ifdef PLATFORM_A
// platform a stuff
                    +5
                    +50
    #endif
    #ifdef PLATFORM_B
/* platform b stuff */
                    +2
    #endif
            );
    buf = alloca(3
    #ifdef PLATFORM_A
// platform a stuff
                    +5
                    +50
    #endif
    #ifdef PLATFORM_B
/* platform b stuff */
                    +2
    #endif
            );
}

int other() {
    buf = alloca(3
    #ifdef PLATFORM_A
// platform a stuff
                    +5
    #endif
    #ifdef PLATFORM_B
/* platform b stuff */
                    +2
    #endif
            );
    buf = alloca(3
                    +5
                    +2
            );
}

int third() {
    buf = alloca(3+5+2);
    buf = alloca(3
                    +5
                    +2
            );
}
