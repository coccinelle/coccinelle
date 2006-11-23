int main(int i) {

  if(check_region(piix,8)){
    printk("error1");
    return -ENODEV;
  }
  if(force_addr) {
    printk("warning1");
  } else if((temp & 1) == 0) {
    if(force) {
      printk("warning2");
    } else { 
      printk("error2");

      return -ENODEV;
    }
  }
  request_region(piix,8);
  printk("done");  
}

