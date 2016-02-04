#define REG_PATTERN_TEST(R, M, W)                                        \
{                                                                        \
	for (pat = 0; pat < sizeof(test)/sizeof(test[0]); pat++) {        \
		if (value != (test[pat] & W & M)) {                       \
			return 1;                                        \
		}                                                        \
	}                                                                \
}


#define REG_PATTERN_TEST2(R, M, W)                                        \
{                                                                        \
	for (pat = 0; pat < sizeof(test)/sizeof(test[0]); pat++) {        \
		if (value != (test[pat] & W & M)) {                       \
		}                                                        \
	}                                                                \
}
