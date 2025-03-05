int main () {
  max = num_var_ranges;
  if (fcount == NULL) {
    fcount = kzalloc(max * sizeof *fcount, GFP_KERNEL);
    if (!fcount)
      return -ENOMEM;
    FILE_FCOUNT(file) = fcount;
  }
}

