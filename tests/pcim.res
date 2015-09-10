int main () {
		ctx->sensePA = dma_map_single(&adapter->dev->dev,
					      cmd->sense_buffer,
					      SCSI_SENSE_BUFFERSIZE,
					      DMA_FROM_DEVICE);
}
