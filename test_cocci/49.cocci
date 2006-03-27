@@
struct acpi_buffer buf;
@@

--- a/drivers/acpi/.../*.c
+++ b/drivers/acpi/.../*.c

- kfree(buf.pointer)
- acpi_os_free(buf.pointer)
