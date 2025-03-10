#define DEFINE_CLK_STUB(_id, _cmd, _name)			\
	{							\
		.id = (_id),					\
		.cmd = (_cmd),					\
		.hw.init = &(struct clk_init_data) {		\
			.name = #_name,				\
			.ops = &hi3660_stub_clk_ops,		\
			.num_parents = 0,			\
			.flags = CLK_GET_RATE_NOCACHE,		\
		},						\
	},

struct hi3660_stub_clk hi3660_stub_clks[HI3660_CLK_STUB_NUM] = {
 	DEFINE_CLK_STUB(HI3660_CLK_STUB_GPU)
 	DEFINE_CLK_STUB(HI3660_CLK_STUB_GPU)
 	DEFINE_CLK_STUB(HI3660_CLK_STUB_GPU)
	DEFINE_CLK_STUB(HI3660_CLK_STUB_DDR)
};

int main () {
  if (x) {
    x = 12;
    x++;
    return 12;
  }
}
