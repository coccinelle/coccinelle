int main () {
  memset(command, 0, sizeof(struct sbp2_command_info));
  memset(command, 0x00, sizeof(struct sbp2_command_info));
  memset(command, 0x0, sizeof(struct sbp2_command_info));
  memset(command, '\0', sizeof(struct sbp2_command_info));
}
