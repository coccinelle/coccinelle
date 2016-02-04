// From: Shani Moideen <shani.moideen@wipro.com>
// Subject: [KJ] [KJ PATCH] Replacing memset(<addr>,0,PAGE_SIZE) with
// 	clear_page(<addr>) in drivers/char/drm/i810_dma.c
//
// Replacing memset(<addr>,0,PAGE_SIZE) with clear_page(<addr>) 
// in drivers/char/drm/i810_dma.c
// 
// Signed-off-by: Shani Moideen <shani.moideen@wipro.com>
// ----
// 
// diff --git a/drivers/char/drm/i810_dma.c b/drivers/char/drm/i810_dma.c
// index 603d17f..4dbd97f 100644
// --- a/drivers/char/drm/i810_dma.c
// +++ b/drivers/char/drm/i810_dma.c
// @@ -413,7 +413,7 @@ static int i810_dma_initialize(drm_device_t * dev,
//  		DRM_ERROR("Can not allocate hardware status page\n");
//  		return -ENOMEM;
//  	}
// -	memset(dev_priv->hw_status_page, 0, PAGE_SIZE);
// +	clear_page(dev_priv->hw_status_page);


// script found on KJ:
// grep -e "memset ?\([^,]+, ?, ?0, ?PAGE_SIZE\) "  

@@
expression X;
@@

-	memset(X, 0, PAGE_SIZE)
+	clear_page(X)

