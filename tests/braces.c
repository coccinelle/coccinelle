#define main { foo(); }

int main() { foo(); }

int main() { if (x) { foo(); } }

int main() { while (x) { foo(); } }

int main() { if (x) { foo(); } else { foo(); } }
