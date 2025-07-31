@@
type T;
identifier buf;
expression lock,E,format1,L,format2;
symbol merged;
expression list args,args_before,args_after;
@@

{
...
-T buf[...];
...
ldap_pvt_thread_mutex_lock(lock);
...
-snprintf( buf, E, format1, args );
+Debug( L, merged, args_before, args, args_after );
...
ldap_pvt_thread_mutex_unlock(lock);
...
-Debug( L, format2, args_before, buf, args_after );
...
}
