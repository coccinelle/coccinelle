#define DC390_AFLAGS unsigned long aflags;


#define ACPI_COMMON_OBJ_INFO \
	acpi_object_type            type;           /* ACPI object type */ \
	acpi_name                   name            /* ACPI object Name */


typedef struct
{
	ACPI_COMMON_OBJ_INFO;
} acpi_obj_info_header;


typedef struct
{
	ACPI_COMMON_OBJ_INFO;

	u32                         valid;              /*  Are the next bits legit? */
	char                        hardware_id[9];     /*  _HID value if any */
	char                        unique_id[9];       /*  _UID value if any */
	acpi_integer                address;            /*  _ADR value if any */
	u32                         current_status;     /*  _STA value */
} acpi_device_info;
