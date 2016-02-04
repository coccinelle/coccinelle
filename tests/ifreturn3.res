int GetExitCode (int iFlag_Code)
{
  if(iFlag_Code==OK) {
    return OK;
  } else // blah
  {
    mwtrace();
    return WARNING;
  }
  return 0;
}

int GetExitCode (int iFlag_Code)
{
  if(iFlag_Code==OK) {
    return OK;
  } else {
    mwtrace();
    return WARNING;
  }
  return 0;
}
