int
main(int argc, char *argv[])
{
// ...
  dpy = XOpenDisplay (displayname);
  if (!dpy) {
    fprintf (stderr, "%s:  unable to open display \"%s\"\n",
	     ProgramName, displayname ? displayname : getenv("DISPLAY"));
    Exit (1);
  }
  screenno = DefaultScreen (dpy);
// ...
}
