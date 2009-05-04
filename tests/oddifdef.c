void one () {
  if (errno != ENOENT 
#ifdef ENOTDIR
      && errno != ENOTDIR
#endif
    )
    a = 5;

#ifdef FOO
  x = 0;
#else
  x = 0;
#endif
}

void two() {
#ifdef ENOTTY
  if (errno == ENOTTY)
    is_a_tty=0;
  else
#endif
    a = 3;

#ifdef FOO
  x = 0;
#else
  x = 0;
#endif
}

void three() {
  if (x)
    a = 3;
#ifndef OPENSSL_NO_SSL2
  else if (strcmp(*argv,"-ssl2") == 0)
    meth=SSLv2_client_method();
#endif

#ifdef FOO
  x = 0;
#else
  x = 0;
#endif
}
