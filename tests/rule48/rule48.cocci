@@
/*acpi_handle handle;
acpi_string pathname;
struct acpi_object_list *external_params;
struct acpi_buffer *return_buffer;*/
expression handle, pathname, external_params, return_buffer;
@@

- acpi_evaluate(handle, pathname, external_params, return_buffer)
+ acpi_evaluate_object(handle, pathname, external_params, return_buffer)
