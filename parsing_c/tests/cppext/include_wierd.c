#include CONFIG_ACPI_CUSTOM_DSDT_FILE


// File "/tmp/linux-2.6.13/drivers/atm/ambassador.c", line 300, characters 0-1:
//    around = '#', whole content = #include UCODE(start)

#ifdef AMB_NEW_MICROCODE
#define UCODE(x) UCODE2(atmsar12.x)
#else
#define UCODE(x) UCODE2(atmsar11.x)
#endif
#define UCODE2(x) #x

static u32 __devinitdata ucode_start =
#include UCODE(start)
;

static region __devinitdata ucode_regions[] = {
#include UCODE(regions)
  { 0, 0 }
};

static u32 __devinitdata ucode_data[] = {
#include UCODE(data)
  0xdeadbeef
};



#define SYSTEM_H "system_1.h"

#include SYSTEM_H

// `SYSTEM_H' will be expanded, and the preprocessor will look for
// `system_1.h' as if the `#include' had been written that way originally.
// `SYSTEM_H' could be defined by your Makefile with a `-D' option.
