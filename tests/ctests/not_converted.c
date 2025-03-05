// -ifdef_to_if doesn't convert this ifdef

ssize_t __net_Write( vlc_object_t *p_this, int fd, const v_socket_t *p_vs,
                     const uint8_t *p_data, size_t i_data )
{
        if (p_vs != NULL)
            val = p_vs->pf_send (p_vs->p_sys, p_data, i_data);
        else
#ifdef WIN32
            val = send (fd, p_data, i_data, 0);
#else
            val = write (fd, p_data, i_data);
#endif
}
