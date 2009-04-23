static int blk_end_io(int (drv_callback)(struct request *))
{
 	if (drv_callback && drv_callback(rq))
		return 1;

	return 0;
}
