void main() {
  x = NULL;
  //(struct us_data*)psh->hostdata[0] = NULL;
  (struct us_data*)psh->hostdata = NULL;
  //psh->hostdata = NULL; //OK
}


