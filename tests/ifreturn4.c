int GetExitCode (int iFlag_Code)
{
  if(iFlag_Code==OK) {
    return OK;
  } else if(iFlag_Code==WARNING) {
    return WARNING;
  } else {
    return_ERREUR;
  }
  return 0;
}
