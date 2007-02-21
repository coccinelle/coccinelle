//parse error 
// = File "test_c/bugs/66/ok/linux-2.6.8/sound/isa/sb/sb_common.c", line 213, characters 7
//    around = 'irqreturn_t', whole content = 		     irqreturn_t (*irq_handler)(int, void *, struct pt_regs *),
// charpos = 4832

int snd_sbdsp_create(snd_card_t *card,
		     unsigned long port,
		     int irq,
 		     irqreturn_t (*irq_handler)(int, void *, struct pt_regs *)
                     );


//EXECUTING: cpp -nostdinc -isystem /usr/lib/gcc-lib/i486-slackware-linux/3.3.4/include -D__GNUC__=3 -D__KERNEL__ -I/tmp/linux-2.6.13/include -I/tmp/linux-2.6.13/include/asm-i386/mach-default -I/tmp/linux-2.6.13/include/asm-ppc/    -I/tmp/linux-2.6.13/drivers/ide -DMODULE -DKBUILD_BASENAME=gscd -DKBUILD_MODNAME=gscd  /tmp/linux-2.6.13/drivers/ide/ide-cd.c> /tmp/main_ml-1.c
//parse error=File "/tmp/main_ml-2.c", line 17221, characters 54-64:
//around = ide_hwif_t, whole content = int probe_hwif_init_with_fixup(ide_hwif_t *, void (*)(ide_hwif_t *));
//charpos = 486434


int probe_hwif_init_with_fixup(ide_hwif_t *, void (*)(ide_hwif_t *));
