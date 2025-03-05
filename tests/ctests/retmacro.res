#define REG_PATTERN_TEST(R, M, W)                                        \
{                                                                        \
	for (pat = 0; pat < ARRAY_SIZE(test); pat++) {        \
		if (value != (test[pat] & W & M)) {                       \
			return 1;                                        \
		}                                                        \
	}                                                                \
}


#define REG_PATTERN_TEST2(R, M, W)                                        \
{                                                                        \
	for (pat = 0; pat < ARRAY_SIZE(test); pat++) {        \
		if (value != (test[pat] & W & M)) {                       \
		}                                                        \
	}                                                                \
}
