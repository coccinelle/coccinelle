int GetExitCode (int iFlag_Code)
{
  if(iFlag_Code==OK) {
    return OK;
  } else {
    mwtrace();
    if(iFlag_Code==WARNING) {
      return WARNING;
    } else {
      mwtrace();
      {
        return_ERREUR;
      }
    }
  }
  return 0;
}
