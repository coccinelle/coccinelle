int main () {
		ctx->sensePA = pci_map_single(adapter->dev, cmd->sense_buffer,
					      SCSI_SENSE_BUFFERSIZE,
					      PCI_DMA_FROMDEVICE);
}
