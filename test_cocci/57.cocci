@@
struct Scsi_Host *host;
struct pci_dev *pdev;
@@

- scsi_set_pci_device(host,pdev)
+ scsi_set_device(host,&pdev->dev)
