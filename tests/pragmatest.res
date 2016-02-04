int two () { return 12; }

int one () __attribute__((always_inline));

int one () { return 12; }

int three () { return 12; }

#pragma abc ddd def

