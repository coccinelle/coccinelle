static int __init test_user_copy_init(void)
{
#ifdef TEST_U64
	u64 val_u64;
#endif
	kmem = kmalloc(PAGE_SIZE * 2, GFP_KERNEL);

#define test_legit(size, check)						  \
	do {								  \
		ret |= test(get_user(val_##size, (size __user *)usermem), \
		    "legitimate get_user (" #size ") failed");		  \
		if (val_##size != check) {				  \
		}							  \
	} while (0)
}
