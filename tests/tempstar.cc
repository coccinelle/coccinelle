bool ForkedProcess::HandleAbnormalExit(pid_t child, bool wait_status) {
  bool handled = false;
  absl::MutexLock lock(&ForkedProcess::mu_);
  absl::flat_hash_map<pid_t, ForkedProcess*>& children =
      ForkedProcess::GetAllChildren();

  auto fpl = children.find(child);
  if (fpl == children.end()) {
    return false;
  }
  ForkedProcess* fp = fpl->second;

  // We want to run all of the handlers.  Any of them can abort the exit.
  for (const auto& handler : fp->exit_handlers_) {
    handled |= handler(child, wait_status);
  }
  return handled;
}
