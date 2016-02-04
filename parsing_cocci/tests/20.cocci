@@
expression A, B, C, D;
@@

- snd_pcm_lib_preallocate_pci_pages_for_all(A, B, C, D)
+ snd_pcm_lib_preallocate_pages_for_all(B, SNDRV_DMA_TYPE_DEV, snd_dma_pci_data(A), C, D)

@@
expression A, B, C;
@@

- snd_pcm_lib_preallocate_isa_pages_for_all(A, B, C)
+ snd_pcm_lib_preallocate_pages_for_all(A, SNDRV_DMA_TYPE_DEV, snd_dma_isa_data(), B, C)

@@
expression A, B, C, D;
@@

- snd_pcm_lib_preallocate_sbus_pages_for_all(A, B, C, D)
+ snd_pcm_lib_preallocate_pages_for_all(B, SNDRV_DMA_TYPE_SBUS, snd_dma_sbus_data(A), C, D)
