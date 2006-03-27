@@
expression X, A, B, C;
@@

- snd_pcm_lib_preallocate_pages_for_all(X,A,B,C)
+ snd_pcm_lib_preallocate_pages_for_all(X, SNDRV_DMA_TYPE_CONTINUOUS, snd_dma_continuous_data(C), A, B)
