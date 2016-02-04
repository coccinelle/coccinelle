struct vma_to_fileoffset_map *create_vma_map(const struct spu *aSpu,
					     unsigned long spu_elf_start)
{
	for (i = 0; i < n_ovlys; i++) {
		map = vma_map_add(map, ovly.vma, ovly.size, ovly.offset,
				  ovly_buf_table_sym + (ovly.buf-1) * 4, i+1);
		if (!map) { map = NULL;
		goto fail; }
	}
	goto out;

 fail:
	map = NULL;
 out:
	return map;
}
