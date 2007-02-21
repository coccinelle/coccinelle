void main(int i)
{
	snd_iprintf(buffer,
		    "Advanced Linux Sound Architecture Driver Version " CONFIG_SND_VERSION CONFIG_SND_DATE ".\n"
		    "Compiled on " __DATE__ " for kernel %s"
                    );
        
}
