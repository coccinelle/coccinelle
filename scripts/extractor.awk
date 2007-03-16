#!/usr/bin/awk -f
#
# Extracting time statistics from .failed/.ok files
#

BEGIN { 
  no_of_files = 0;
  max_running_time = 0.0;
  max_running_file = "";
  min_running_time = 100000.0;
  min_running_file = "";
  sum_running_time = 0.0;

  min_file_size_lines = 0;
  max_file_size_lines = 0;
  tot_file_size_lines = 0;
  min_file_size_bytes = 0;
  max_file_size_bytes = 0;
  tot_file_size_bytes = 0;
}
/real[ \t]+[0-9]+m[0-9]+[.][0-9]+/{

  # The file currently processed
  current_file = FILENAME;

  # Count the number of files
  no_of_files++;

  # Parse time field
  split($2,timearr,/[m.]/);
  current_time = timearr[1] * 60.0;   # minutes
  current_time += timearr[2];         # seconds
  current_time += timearr[3] / 1000.0; # 1/1000th sec.

  # Print progress
  printf "%6.3fs (%s)\n", current_time, current_file;

  # Update total time
  sum_running_time += current_time;

  # Update max and min
  if(current_time > max_running_time)
    {
      max_running_time = current_time;
      max_running_file = current_file;
    }
  if(current_time < min_running_time)
    {
      min_running_time = current_time;
      min_running_file = current_file;
    }

  # Find corresponding .c file
  gsub(/.(ok|failed|spatch_ok|gave_up)$/,".c",current_file);

  # Update file sizes (in lines)  
  ("wc -l " current_file) | getline;
  current_size_lines = $1;
  tot_file_size_lines += current_size_lines;
  if(current_size_lines > max_file_size_lines)
    {
      max_file_size_lines = current_size_lines;
    }
  if(current_size_lines < min_file_size_lines)
    {
      min_file_size_lines = current_size_lines;
    }

  # Update file sizes (in bytes)  
  ("du " current_file) | getline;
  current_size_bytes = $1;
  tot_file_size_bytes += current_size_bytes;
  if(current_size_bytes > max_file_size_bytes)
    {
      max_file_size_lines = current_size_bytes;
    }
  if(current_size_bytes < min_file_size_bytes)
    {
      min_file_size_bytes = current_size_bytes;
    }
}
END {
  printf "!!No. of files    : %6d\n", no_of_files;
  printf "  Total # of lines: %6d\n", tot_file_size_lines;
  printf "!!Avg. # of lines : %9.2f\n", (tot_file_size_lines / no_of_files);
  printf "  Total size (KB) : %6d\n", tot_file_size_bytes;
  printf "  Avg. size (KB)  : %9.2f\n", (tot_file_size_bytes / no_of_files);
  printf "  Minimum time    : %9.2fs (%s)\n", 
    min_running_time, min_running_file;
  printf "!!Maximum time    : %9.2fs (%s)\n", 
    max_running_time, max_running_file;
  printf "  Total time      : %9.2fs\n", sum_running_time;
  printf "!!Average time    : %9.2fs\n", (sum_running_time / no_of_files);
}
