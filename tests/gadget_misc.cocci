// author: Michele Martone
// description: disable semi-intentional segfault

@Gravity__forcetree_update_c@
@@
- *((int *) (0x0)) = 1;
//+ /* *((int *) (0x0)) = 1; */

// vim:number:syntax=diff
