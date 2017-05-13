int deadcode(const char* c)
{
    do {
        if (c) {
            foo(*c);
            break;
        }
        break;
    } while (0);
}
