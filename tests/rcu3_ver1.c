int cryptocop_free_session(cryptocop_session_id sid)
{
  struct list_head                  *node, *tmp;

  for (i = 0; i < cryptocop_prio_no_prios; i++){
    if (!list_empty(&(cryptocop_job_queues[i].jobs))){
      list_for_each_safe(f(node), _Y(tmp), &(cryptocop_job_queues[i].jobs)) {
	pj = list_entry(_Y(node), struct cryptocop_prio_job, _Y(node));
	if (pj->oper->sid == sid) {
	  list_move_tail(_Y(node), &remove_list);
	}
      }
    }
  }
}
