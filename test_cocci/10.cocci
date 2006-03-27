@@
expression E1, E2, E3, E4;
@@

(
- acpi_hw_low_level_read(E1, E2, E3, E4)
+ acpi_hw_low_level_read(E1, E2, E3)
|
- acpi_hw_low_level_write(E1, E2, E3, E4)
+ acpi_hw_low_level_write(E1, E2, E3)
)
