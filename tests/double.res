static void BChannel_proc_xmt(struct BCState *bcs) {
   if (!test_bit(BC_FLG_BUSY, &bcs->Flag) && skb_queue_empty(&bcs->squeue)) {
     st->l2.l2l1(st, PH_DEACTIVATE | CONFIRM, NULL);
   }
}
