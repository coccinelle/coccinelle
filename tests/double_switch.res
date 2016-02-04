
static NTSTATUS get_line_control(int fd, SERIAL_LINE_CONTROL* slc)
{
#ifdef CMSPAR
    switch (port.c_cflag & (PARENB | PARODD | CMSPAR))
#else
    switch (port.c_cflag & (PARENB | PARODD))
#endif
    {
    case 0:                     slc->Parity = NOPARITY;         break;
    case PARENB:                slc->Parity = EVENPARITY;       break;
    case PARENB|PARODD:         slc->Parity = ODDPARITY;        break;
    }
    return STATUS_SUCCESS;
}
