int deadcode(const char* c)
{
    do {
        if (c) {
            bar(*c);
            break;
        }
        break;
    } while (0);
}
