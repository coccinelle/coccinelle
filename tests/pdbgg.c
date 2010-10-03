/* regression if remove the definintion of PDBGG in standard.h: 

Great: a test file now works: /home/pad/linux/arch/mips/alchemy/common/au1xxx_irqmap.c
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/arch/powerpc/boot/ps3.c
Error : bad = 11, timeout = false
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/i2c/busses/i2c-ibm_iic.c
Error : bad = 52, timeout = false
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/media/video/et61x251/et61x251_core.c
Error : bad = 197, timeout = false
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/media/video/sn9c102/sn9c102_core.c
Error : bad = 247, timeout = false
Great: a test file now works: /home/pad/linux/drivers/media/video/w9968cf.c
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/media/video/zc0301/zc0301_core.c
Error : bad = 96, timeout = false
Great: a test file now works: /home/pad/linux/drivers/mtd/devices/docprobe.c
Great: a test file now works: /home/pad/linux/drivers/mtd/nand/diskonchip.c
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/net/ibm_newemac/core.c
Error : bad = 14, timeout = false
Great: a test file now works: /home/pad/linux/drivers/scsi/in2000.c
Great: a test file now works: /home/pad/linux/drivers/scsi/scsi_lib.c
Great: a test file now works: /home/pad/linux/drivers/scsi/wd33c93.c
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/usb/gadget/composite.c
Error : bad = 173, timeout = false
Semipb: still error but not same error : /home/pad/linux/drivers/usb/gadget/omap_udc.c
Old error: bad = 1, timeout = fals
New error: bad = 115, timeout = false
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/usb/gadget/printer.c
Error : bad = 168, timeout = false
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/usb/musb/cppi_dma.c
Error : bad = 20, timeout = false
Semipb: still error but not same error : /home/pad/linux/drivers/usb/musb/musb_core.c
Old error: bad = 2, timeout = fals
New error: bad = 315, timeout = false
PBBBBBBBB: a test file does not work anymore!!! : /home/pad/linux/drivers/usb/musb/musb_gadget.c
Error : bad = 104, timeout = false
Great: a test file now works: /home/pad/linux/kernel/auditfilter.c
Great: a test file now works: /home/pad/linux/sound/soc/sh/hac.c
Great: a test file now works: /home/pad/linux/sound/soc/sh/ssi.c


cp /home/pad/linux/arch/mips/alchemy/common/au1xxx_irqmap.c /home/pad/linux/arch/powerpc/boot/ps3.c /home/pad/linux/drivers/i2c/busses/i2c-ibm_iic.c /home/pad/linux/drivers/media/video/et61x251/et61x251_core.c /home/pad/linux/drivers/media/video/sn9c102/sn9c102_core.c /home/pad/linux/drivers/media/video/w9968cf.c /home/pad/linux/drivers/media/video/zc0301/zc0301_core.c /home/pad/linux/drivers/mtd/devices/docprobe.c /home/pad/linux/drivers/mtd/nand/diskonchip.c /home/pad/linux/drivers/net/ibm_newemac/core.c /home/pad/linux/drivers/scsi/in2000.c /home/pad/linux/drivers/scsi/scsi_lib.c /home/pad/linux/drivers/scsi/wd33c93.c /home/pad/linux/drivers/usb/gadget/composite.c /home/pad/linux/drivers/usb/gadget/omap_udc.c /home/pad/linux/drivers/usb/gadget/printer.c /home/pad/linux/drivers/usb/musb/cppi_dma.c /home/pad/linux/drivers/usb/musb/musb_core.c /home/pad/linux/drivers/usb/musb/musb_gadget.c /home/pad/linux/kernel/auditfilter.c /home/pad/linux/sound/soc/sh/hac.c /home/pad/linux/sound/soc/sh/ssi.c


*/

int main() {
  PDBGG("this is a test %d\n",x);
}

int main() {
  PDBGG("this is a test %d\n",x)
}

int main()
{
	PDBGG(static unsigned int prev_mask = 0);
}


int main() {

           PDBGG("Isochrnous frame: length %u, #%u i", len, i)

           /*
              NOTE: It is probably correct to assume that SOF and EOF
                    headers do not occur between two consecutive packets,
                    but who knows..Whatever is the truth, this assumption
                    doesn't introduce bugs.
           */

redo:
           sof = sn9c102_find_sof_header(cam, pos, len);

}
