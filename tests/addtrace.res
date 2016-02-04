short VerDate(char *pcDate)
{
  short sRetour = 0;
  if ( pcDate == 0 ) {
    trace("ifth");
    {
      sRetour = 1;
    }
  }
  trace("endif");
  if ( pcDate == 0 ) {
    trace("ifth");
    sRetour = 1;
  }
  trace("endif");
  if ( pcDate == 0 ) {
    trace("ifth");
    {
      sRetour = 1;
      return sRetour;
    }
  }
  trace("endif");
  return sRetour;
}
