struct cword {
 	unsigned int __attribute__ ((__packed__))
		rounds:4,
		algo:3,
		keygen:1,
		interm:1,
		encdec:1,
		ksize:2;
} __attribute__ ((__aligned__(PADLOCK_ALIGNMENT)));
