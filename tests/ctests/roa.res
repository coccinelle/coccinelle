struct found used __ro_after_init = {
  .delay          = __loop_delay,
  .const_udelay   = __loop_const_udelay,
  .udelay         = __loop_udelay,
};

union arm_delay_ops2 not_used = {
  .delay          = __loop_delay,
  .const_udelay   = __loop_const_udelay,
  .udelay         = __loop_udelay,
};
