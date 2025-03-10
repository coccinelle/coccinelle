void zfLnxRecvEth(zdev_t* dev, zbuf_t* buf, u16_t port)
{
#ifdef ZM_AVOID_UDP_LARGE_PACKET_FAIL
    zbuf_t *new_buf;

    switch(netif_rx(new_buf))
#else

    switch(netif_rx(buf))
#endif
    {
    case NET_RX_BAD:
        break;
    }

    return;
}
