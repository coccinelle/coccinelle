@@
expression Ename, Eid, Eres, Enum_res, Edata, Esize_data, Edma_mask;
@@

-imx_add_platform_device_dmamask(Ename, Eid, Eres, Enum_res, Edata, Esize_data, Edma_mask)
+platform_device_register_full(&((struct platform_device_info){ .name = Ename, .id = Eid, .res = Eres, .num_res = Enum_res, .data = Edata, .size_data = Esize_data, .dma_mask = Edma_mask, }))

@@
expression Ename, Eid, Eres, Enum_res, Edata, Esize_data, Edma_mask;
@@

-platform_device_register_full(&((struct platform_device_info){ .name = Ename, .id = Eid, .res = Eres, .num_res = Enum_res, .data = Edata, .size_data = Esize_data, .dma_mask = Edma_mask, }))
+changed_imx_add_platform_device_dmamask(Ename, Eid, Eres, Enum_res, Edata, Esize_data, Edma_mask)
