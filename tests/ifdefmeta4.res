int main() {
    buf = malloc(3
#ifdef PLATFORM_A
                    // platform a stuff
                     + 5 + 50
#endif
#ifdef PLATFORM_B
                    /* platform b stuff */
                     + 2
#endif
            );
}

