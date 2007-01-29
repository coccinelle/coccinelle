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

// not clear what the following has to do with the above CE, but it occurs in
// a number of files

@@
expression obj_desc, walk_state;
@@

  acpi_ex_truncate_for32bit_table (obj_desc
-                                          , walk_state
                                  )
// need to do the following, but switch is not in smpl
//@@
//@@
//
//	switch (ACPI_GET_OBJECT_TYPE(stack_desc)) {
//                ...
//-               case AML_ZERO_OP:
//-               case AML_ONE_OP:
//-               case AML_ONES_OP:
//-               case AML_REVISION_OP:
//-               ...
//(
//                case I:
//|
//                default:
//)
//                ...
//         }
//  hmm can't rely on the order, so would need four separate rules for this