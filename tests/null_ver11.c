   void udf_fill_spartable(struct super_block *sb, struct udf_sparing_data *sdata, int partlen)
   {
   	Uint16 ident;
   	Uint32 spartable;
   	int i;
   	struct buffer_head *bh;
   	struct SparingTable *st;
   
   	for (i=0; i<4; i++)
   	{
   		if (!(spartable = sdata->s_spar_loc[i]))
   			continue;
   
   		bh = udf_read_tagged(sb, spartable, spartable, &ident);
   
   		if (!bh)
   		{
   			sdata->s_spar_loc[i] = 0;
   			continue;
   		}
   
   		if (ident == 0)
   		{
   			st = (struct SparingTable *)bh->b_data;
   			if (!strncmp(st->sparingIdent.ident, UDF_ID_SPARING, strlen(UDF_ID_SPARING)))
   			{
   				SparingEntry *se;
   				Uint16 rtl = le16_to_cpu(st->reallocationTableLen);
   				int index;
   
   				if (!sdata->s_spar_map)
   				{
   					int num = 1, mapsize;
   					sdata->s_spar_indexsize = 8;
   					while (rtl*sizeof(Uint32) >= (1 << sdata->s_spar_indexsize))
   					{
   						num ++;
   						sdata->s_spar_indexsize <<= 1;
   					}
   					mapsize = (rtl * sizeof(Uint32)) +
   						((partlen/(1 << sdata->s_spar_pshift)) * sizeof(Uint8) * num);
   					sdata->s_spar_map = kmalloc(mapsize, GFP_KERNEL);
   					sdata->s_spar_remap.s_spar_remap32 = &sdata->s_spar_map[rtl];
   					memset(sdata->s_spar_map, 0xFF, mapsize);
   				}
   
   				index = sizeof(struct SparingTable);
   				for (i=0; i<rtl; i++)
   				{
   					if (index > sb->s_blocksize)
   					{
   						udf_release_data(bh);
   						bh = udf_tread(sb, ++spartable, sb->s_blocksize);
   						if (!bh)
   						{
   							sdata->s_spar_loc[i] = 0;
   							continue;
   						}
   						index = 0;
   					}
   					se = (SparingEntry *)&(bh->b_data[index]);
   					index += sizeof(SparingEntry);
   
   					if (sdata->s_spar_map[i] == 0xFFFFFFFF)
   						sdata->s_spar_map[i] = le32_to_cpu(se->mappedLocation);
   					else if (sdata->s_spar_map[i] != le32_to_cpu(se->mappedLocation))
   					{
   						udf_debug("Found conflicting Sparing Data (%d vs %d for entry %d)\n",
   							sdata->s_spar_map[i], le32_to_cpu(se->mappedLocation), i);
   					}
   
   					if (le32_to_cpu(se->origLocation) < 0xFFFFFFF0)
   					{
   						int packet = le32_to_cpu(se->origLocation) >> sdata->s_spar_pshift;
   						if (sdata->s_spar_indexsize == 8)
   						{
   							if (sdata->s_spar_remap.s_spar_remap8[packet] == 0xFF)
   								sdata->s_spar_remap.s_spar_remap8[packet] = i;
   							else if (sdata->s_spar_remap.s_spar_remap8[packet] != i)
   							{
   								udf_debug("Found conflicting Sparing Data (%d vs %d)\n",
   									sdata->s_spar_remap.s_spar_remap8[packet], i);
   							}
   						}
   						else if (sdata->s_spar_indexsize == 16)
   						{
   							if (sdata->s_spar_remap.s_spar_remap16[packet] == 0xFFFF)
   								sdata->s_spar_remap.s_spar_remap16[packet] = i;
   							else if (sdata->s_spar_remap.s_spar_remap16[packet] != i)
   							{
   								udf_debug("Found conflicting Sparing Data (%d vs %d)\n",
   									sdata->s_spar_remap.s_spar_remap16[packet], i);
   							}
   						}
   						else if (sdata->s_spar_indexsize == 32)
   						{
   							if (sdata->s_spar_remap.s_spar_remap32[packet] == 0xFFFFFFFF)
   								sdata->s_spar_remap.s_spar_remap32[packet] = i;
   							else if (sdata->s_spar_remap.s_spar_remap32[packet] != i)
   							{
   								udf_debug("Found conflicting Sparing Data (%d vs %d)\n",
   									sdata->s_spar_remap.s_spar_remap32[packet], i);
   							}
   						}
   					}
   				}
   			}
   		}
   		udf_release_data(bh);
   	}
 }

