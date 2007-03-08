static int AztTimeout, AztTries;
static DECLARE_WAIT_QUEUE_HEAD(azt_waitq);
static DEFINE_TIMER(delay_timer, NULL, 0, 0);

