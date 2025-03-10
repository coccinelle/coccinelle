qboolean SV_ReadClientMessage (void)
{
	int		ret;
	int		cmd;
	char		*s;
	
				if (host_client->privileged)
					ret = 2;
				else
					ret = 0;
				if (Q_strncasecmp(s, "status", 6) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "god", 3) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "notarget", 8) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "fly", 3) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "name", 4) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "noclip", 6) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "say", 3) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "say_team", 8) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "tell", 4) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "color", 5) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "kill", 4) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "pause", 5) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "spawn", 5) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "begin", 5) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "prespawn", 8) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "kick", 4) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "ping", 4) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "give", 4) == 0)
					ret = 1;
				else if (Q_strncasecmp(s, "ban", 3) == 0)
					ret = 1;
				if (ret == 2)
					Cbuf_InsertText (s);
				else if (ret == 1)
					Cmd_ExecuteString (s, src_client);
				else
					Con_DPrintf("%s tried to %s\n", host_client->name, s);
				
}

