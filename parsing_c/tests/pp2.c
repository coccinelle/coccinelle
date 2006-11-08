static const char *mrw_format_status[];

static int open_for_data(struct cdrom_device_info * cdi, int i);


static void cdrom_update_settings(void)
{
	struct cdrom_device_info *cdi;

	for (cdi = topCdromPtr; cdi != NULL; cdi = cdi->next) {
		if (autoclose && CDROM_CAN(CDC_CLOSE_TRAY))
			cdi->options |= CDO_AUTO_CLOSE;
		else if (!autoclose)
			cdi->options &= ~CDO_AUTO_CLOSE;
		if (autoeject && CDROM_CAN(CDC_OPEN_TRAY))
			cdi->options |= CDO_AUTO_EJECT;
		else if (!autoeject)
			cdi->options &= ~CDO_AUTO_EJECT;
		if (lockdoor && CDROM_CAN(CDC_LOCK))
			cdi->options |= CDO_LOCK;
		else if (!lockdoor)
			cdi->options &= ~CDO_LOCK;
		if (check_media_type)
			cdi->options |= CDO_CHECK_TYPE;
		else
			cdi->options &= ~CDO_CHECK_TYPE;
	}
}


/* simple example */
static void f() {
  int *j;
  int j[];

  char **start;
   struct cdrom_device_info *cdi, *prev;

  if(g(h(3))) {
    foo();
    x = 1;
  } else {
    bar();
    x = 2;
    goto error;
    //x = 3;  //  must generate exception DeadCode
  }
  foobar();
  foobar();
  while(1) {
    foo1();
    
  }
  if(1) {
    return 3;
  }

out:
  foo2();

error:
  foo3();
  goto last; // would generate exception DeadCode too (with first (buggy) version of deadcode detection)

last:
  foo4();
  foo5(TOTO, "toto\n");

  return; // was returning deadcode with first (buggy) version

}


void main(int o) {
  f();
}
