static void mcast_work_handler(struct work_struct *work)
{
	struct mcast_group *group;
	struct mcast_member *member;
	struct ib_sa_multicast *multicast;
	int status, ret;
	u8 join_state;

 	group = container_of(work, typeof(*group), work);
}
