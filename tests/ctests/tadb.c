static int adbhid_kbd_event()
{
}


static void
adbhid_input_register()
{

		adbhid[id]->input.event = adbhid_kbd_event;

}
