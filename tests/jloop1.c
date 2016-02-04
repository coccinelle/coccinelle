void cpu_idle(void)   
{
   local_fiq_enable();
   
   /* endless idle loop with no priority at all */
   while (1) {
     int idle = pm_idle;
     if (!idle)
       idle = default_idle;
     preempt_disable();
     leds_event(led_idle_start);
     while (!need_resched())
       idle();
     leds_event(led_idle_end);
     preempt_enable();
     schedule();
   }
}
