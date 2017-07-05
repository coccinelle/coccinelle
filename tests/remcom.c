int
asyncmeta_retry(
	Operation		*op )
{
	if ( LogTest( LDAP_DEBUG_ANY ) ) {
		char	buf[ SLAP_TEXT_BUFLEN ];

		/* this lock is required; however,
		 * it's invoked only when logging is on */
			ldap_pvt_thread_mutex_lock( &mt->mt_uri_mutex );
			snprintf( buf, sizeof( buf ),
				  "retrying URI=\"%s\" DN=\"%s\"",
				  mt->mt_uri,
				  BER_BVISNULL( &msc->msc_bound_ndn ) ?
				  "" : msc->msc_bound_ndn.bv_val );
			ldap_pvt_thread_mutex_unlock( &mt->mt_uri_mutex );

			Debug( LDAP_DEBUG_ANY,
			       "%s asyncmeta_retry[%d]: %s.\n",
			       op->o_log_prefix, candidate, buf );
	}
}
