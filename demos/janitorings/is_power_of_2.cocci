// From: vignesh babu <vignesh.babu@wipro.com>
// Subject: [KJ] [PATCH]is_power_of_2-ntfs
// To: aia21@cantab.net
// Cc: linux-ntfs-dev@lists.sourceforge.net,
//         Kernel Janitors List <kernel-janitors@lists.osdl.org>,
//         linux-kernel <linux-kernel@vger.kernel.org>
// Date: Thu, 14 Jun 2007 13:39:04 +0530
// Organization: WIPRO Technologies
// Reply-To: vignesh.babu@wipro.com
// 
// 
// Replacing (n & (n-1)) in the context of power of 2 checks
// with is_power_of_2
// 
// Signed-off-by: vignesh babu <vignesh.babu@wipro.com>
// --- 
// diff --git a/fs/ntfs/inode.c b/fs/ntfs/inode.c
// index b532a73..8152f79 100644
// --- a/fs/ntfs/inode.c
// +++ b/fs/ntfs/inode.c
// @@ -27,6 +27,7 @@
//  #include <linux/pagemap.h>
//  #include <linux/quotaops.h>
//  #include <linux/slab.h>
// +#include <linux/log2.h>
//  
//  #include "aops.h"
//  #include "attrib.h"
// @@ -1574,7 +1575,7 @@ static int ntfs_read_locked_index_inode(struct inode *base_vi, struct inode *vi)
//  	ntfs_debug("Index collation rule is 0x%x.",
//  			le32_to_cpu(ir->collation_rule));
//  	ni->itype.index.block_size = le32_to_cpu(ir->index_block_size);
// -	if (ni->itype.index.block_size & (ni->itype.index.block_size - 1)) {
// +	if (!is_power_of_2(ni->itype.index.block_size)) {
//  		ntfs_error(vi->i_sb, "Index block size (%u) is not a power of "
//  				"two.", ni->itype.index.block_size);


// how deal with extra '()'

// -		while ((big_pow2 & (big_pow2 - 1)) != 0)
// +		while (!is_power_of_2(big_pow2))
// 
// 
// -	if (ubi->min_io_size == 0 ||
// -	    (ubi->min_io_size & (ubi->min_io_size - 1))) {
// +	if (!is_power_of_2(ubi->min_io_size)) {
// 
// -		if ((arg & (arg-1)) != 0 || arg < 1) {
// +		if (!is_power_of_2(arg)) {
// 
// 
// // do something general for those != 0 ? always redundant ?
// -	if (bsize < 512 || bsize > 4096 || (bsize & (bsize - 1)) != 0)
// +	if (bsize < 512 || bsize > 4096 || !is_power_of_2(bsize))
// 
// -		if (!bits || (bits & (bits - 1)))
// +		if (!is_power_of_2(bits))
// 
// 
// -	J_ASSERT ((hash_size & (hash_size-1)) == 0);
// +	J_ASSERT (is_power_of_2(hash_size));
// 
// -		if ((new_size & (new_size - 1)) != 0) {
// +		if (!is_power_of_2(new_size)){
// 
//  #include "xfs_quota.h"
//  #include "xfs_acl.h"
//  
// +#include <linux/log2.h>
// 
// 
// -	return !(size % (PAGE_SIZE >> 9) || (size & (size - 1)) ||
// +	return !(size % (PAGE_SIZE >> 9) || !is_power_of_2(size) ||



// script found on KJ:
// grep -e "([^\(\)]+) ?\& ?\(\1 ?- ?1\)"


@ rule1 @
expression n;
@@

- n & (n-1)
+ !is_power_of_2(n)

@ rule2 depends on rule1 @
@@

  #include <linux/...>
+ #include <linux/log2.h>
