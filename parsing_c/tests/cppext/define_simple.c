
typedef int snd_usb_stream_t;

#define chip_t snd_usb_stream_t


#define foo   do { 1; } while(0)

#define	MXSER_VERSION	"1.8"
#define	MXSERMAJOR	 174
#define	MXSERCUMAJOR	 175

#define bytein(addr) inb(addr)


#define IRQ_T(info) ((info->flags & ASYNC_SHARE_IRQ) ? SA_SHIRQ : SA_INTERRUPT)


void main(int i)
{
}
