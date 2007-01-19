@@
acpi_operand_object *E;
expression E1;
@@

(
  E->common.type = E1
|
- acpi_ut_get_type_name(E->common.type)
+ acpi_ut_get_object_type_name(E)
|
- E->common.type
+ ACPI_GET_OBJECT_TYPE(E)
)
