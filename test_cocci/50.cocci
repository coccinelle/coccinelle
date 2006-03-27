@@
union acpi_operand_object *obj;
expression E;
@@

(
- acpi_ut_get_type_name(obj->common.type)
+ acpi_ut_get_object_type_name(obj)
|
  obj->common.type = E
|
- obj->common.type
+ ACPI_GET_OBJECT_TYPE(obj)
)
