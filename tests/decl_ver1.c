static int volconvert(int level)
{
	level>>=14;	 	/* Map 16bits down to 2 bit */
 	level&=3;
	
	/* convert to card-friendly values */
	switch (level) 
	{
		case 0: 
			return 0;
		case 1: 
			return 1;
		case 2:
			return 4;
		case 3:
			return 5;
	}
	return 0;	/* Quieten gcc */
}

