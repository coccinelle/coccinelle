

DRIVER_ATTR(opts, S_IRUGO | S_IWUSR, sdebug_opts_show, 
	    sdebug_opts_store)

static ssize_t sdebug_num_tgts_show(struct device_driver * ddp, char * buf) 
{
        return snprintf(buf, PAGE_SIZE, "%d\n", scsi_debug_num_tgts);
}


BUILD_SHOW_FUNC_INT(sensor1_temperature,	 (read_reg(thermostat, TEMP_REG[1])))
BUILD_SHOW_FUNC_INT(sensor2_temperature,	 (read_reg(thermostat, TEMP_REG[2])))
BUILD_SHOW_FUNC_INT(sensor1_limit,		 thermostat->limits[1])
