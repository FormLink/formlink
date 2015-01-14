Off Statistics;

#ifndef `PIPES_'
	#message "No pipes found";
	.end;
#endif

#if (`PIPES_' <= 0)
	#message "No pipes found";
	.end;
#endif

#procedure put(fmt, mexp)
	#toexternal `fmt', `mexp'
	#toexternal "#THE-END-MARK#"
#endprocedure

#setexternal `PIPE1_';
#toexternal "OK"
#fromexternal
.end