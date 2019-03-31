static const u32 nct7904_in_config[] = {
	HWMON_I_INPUT,                  /* dummy, skipped in is_visible */
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	HWMON_I_INPUT,
	0
};

static const struct hwmon_channel_info nct7904_in = {
	.type = hwmon_in,
	.config = nct7904_in_config,
};

static const u32 nct7904_fan_config[] = {
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	HWMON_F_INPUT,
	0
};

static const struct hwmon_channel_info nct7904_fan = {
	.type = hwmon_fan,
	.config = nct7904_fan_config,
};

static const u32 nct7904_pwm_config[] = {
	HWMON_PWM_INPUT | HWMON_PWM_ENABLE,
	HWMON_PWM_INPUT | HWMON_PWM_ENABLE,
	HWMON_PWM_INPUT | HWMON_PWM_ENABLE,
	HWMON_PWM_INPUT | HWMON_PWM_ENABLE,
	0
};

static const struct hwmon_channel_info nct7904_pwm = {
	.type = hwmon_pwm,
	.config = nct7904_pwm_config,
};

static const u32 nct7904_temp_config[] = {
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	HWMON_T_INPUT,
	0
};

static const struct hwmon_channel_info nct7904_temp = {
	.type = hwmon_temp,
	.config = nct7904_temp_config,
};

 static const struct hwmon_channel_info *nct7904_info[] = {
	&nct7904_in,
	&nct7904_fan,
	&nct7904_pwm,
	&nct7904_temp,
 	NULL
 };
