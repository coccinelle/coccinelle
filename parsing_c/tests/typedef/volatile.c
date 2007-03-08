static unsigned short
snd_au1000_ac97_read(struct snd_ac97 *ac97, unsigned short reg)
{
	struct snd_au1000 *au1000 = ac97->private_data;
	u32 volatile cmd;
 	u16 volatile data;
	int             i;
}
