static void task_kill_later(struct asd_ascb *ascb)
{
	struct asd_ha_struct *asd_ha = ascb->ha;
	struct sas_ha_struct *sas_ha = &asd_ha->sas_ha;
	struct Scsi_Host *shost = sas_ha->core.shost;
	struct sas_task *task = ascb->uldd_task;

	INIT_WORK(&task->abort_work, sas_task_abort);
	queue_work(shost->work_q, &task->abort_work);
}
