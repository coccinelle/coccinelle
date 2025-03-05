struct foo {
#ifndef KD_NEW_ORDER_IN_PARTICLE_STRUCTURE
  ALIGN(32) MyFloat Pos[3];   /*!< particle position at its current time */
#endif
};
