#define Cygwin

#include "mathlink.h"
//according to http://stackoverflow.com/questions/977233/warning-incompatible-implicit-declaration-of-built-in-function-xyz
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

extern int FormInit(void);
extern int FormWrite(void);
extern int FormPrompt(void);
extern void FormRead(void);

int main(int argc, char* argv[])
{
	return MLMain(argc, argv);
}

int inited = 0;
int io[2][2];
int stdo[2];
int pid;

int FormInit() {
	const unsigned char *formfullpath, *exargs;
	unsigned char *ffp, *ea;
	int length;
	
	MLGetByteString(stdlink, &formfullpath, &length, 0);
	ffp = (unsigned char*)malloc((length+1)*sizeof(char));
	sprintf(ffp, "%s\0", formfullpath);
	MLReleaseByteString(stdlink, formfullpath, length);
	
	if(MLGetByteString(stdlink, &exargs, &length, 0)!=0) {
		ea = (unsigned char*)malloc((length+1)*sizeof(char));
		sprintf(ea, "%s\0", exargs);
		MLReleaseByteString(stdlink, exargs, length);
	}
	
	if(inited > 0) { 
		close(io[0][0]);
		close(io[0][1]);
		close(io[1][0]);
		close(io[1][1]);
		close(stdo[0]);
		close(stdo[1]);
		kill(pid, SIGTERM);
	} else {
		inited = 1;
	}
	
	pipe(io[0]);
	pipe(io[1]);
	pipe(stdo);

	pid = fork(); 
	if (pid == 0) { 
		close(io[0][1]);
		close(io[1][0]);
		close(stdo[0]); 
		dup2(stdo[1], 1);

#ifdef Cygwin
		char buffer[128]; 				
		sprintf(buffer, "%d,%d\0", io[0][0], io[1][1]);
//RM: change execlp to execl, see http://stackoverflow.com/a/597833/887505
		if(length>0) execl(ffp, ffp, ea, "-pipe", buffer, "init", '\0');
		else execl(ffp, ffp, "-pipe", buffer, "init", '\0');
//
#else
		char buffer[256]; 				
		sprintf(buffer, "%s %s -pipe %d,%d init", ffp, ea, io[0][0], io[1][1]);
		system(buffer); 
#endif     
		free(ffp);
		if(ea!=NULL) free(ea);
		close(io[0][0]);
		close(io[1][1]);
		close(stdo[1]);
	} else {
		free(ffp);
		if(ea!=NULL) free(ea);
		close(io[0][0]);
		close(io[1][1]);
		close(stdo[1]);
		fcntl(stdo[0], F_SETFL, fcntl(stdo[0], F_GETFL, 0) | O_NONBLOCK);
		
		char buffer[1024];
		read(io[1][0], buffer, sizeof(buffer)); 
		char* p = strstr(buffer, "\n");
		if(p==NULL) return -1;
		sprintf(p, ",%d\n\n\0", pid);
		write(io[0][1], buffer, strlen(buffer));
		read(io[1][0], buffer, sizeof(buffer)); 
		p = strstr(buffer, "OK");
		if(p==NULL || p!=buffer) return -1;
		return 0;
	}
}

int FormWrite(void) {	
	const unsigned char *script;
	int length;
	MLGetByteString(stdlink, &script, &length, 0);
	write(io[0][1], script, length);
	write(io[0][1], "\n", 1);
	MLReleaseByteString(stdlink, script, length);
	return 0;
}

int CheckErrorStatus() {
	const int bytes_at_a_time = 1024;
	char *read_buffer = NULL;
	int buffer_size = 0;
	int buffer_offset = 0;
	int chars_io = 1;

	while (chars_io > 0) {
		if (buffer_offset + bytes_at_a_time >= buffer_size) {
			buffer_size = bytes_at_a_time + buffer_size;
			read_buffer = realloc(read_buffer, buffer_size);
		}
		chars_io = read(stdo[0], read_buffer + buffer_offset, bytes_at_a_time); 
		if(chars_io>0) buffer_offset += chars_io;
		else break;
	}

	if(buffer_offset>0) {
		*(read_buffer+buffer_offset) = '\0';
		//if(strstr(read_buffer, "-->") || strstr(read_buffer, "==>") || strstr(read_buffer, "===")) {
			/*Plain String
			MLPutFunction(stdlink, "EvaluatePacket", 1);
			MLPutFunction(stdlink, "WriteString", 2);
			MLPutString(stdlink, "stdout");
			MLPutByteString(stdlink, read_buffer, buffer_offset);
			MLEndPacket(stdlink);
			MLNextPacket(stdlink);
			MLNewPacket(stdlink);
			*/
			MLPutFunction(stdlink, "EvaluatePacket", 1);
			MLPutNext(stdlink, MLTKFUNC);
			MLPutArgCount(stdlink, 1);
			MLPutSymbol(stdlink, "Print");
			MLPutNext(stdlink, MLTKFUNC);
			MLPutArgCount(stdlink, 2);
			MLPutSymbol(stdlink, "Style");
			MLPutString(stdlink, "FORM failed, Please check the following output: ");
			MLPutSymbol(stdlink, "Red");
			
			MLEndPacket(stdlink);
			MLNextPacket(stdlink);
			MLNewPacket(stdlink);
			
			MLPutFunction(stdlink, "EvaluatePacket", 1);
			MLPutNext(stdlink, MLTKFUNC);
			MLPutArgCount(stdlink, 1);
			MLPutSymbol(stdlink, "Print");
			MLPutNext(stdlink, MLTKFUNC);
			MLPutArgCount(stdlink, 2);
			MLPutSymbol(stdlink, "Style");
			MLPutNext(stdlink, MLTKSTR);
			MLPutSize(stdlink, buffer_offset);
			MLPutData(stdlink, read_buffer, buffer_offset);
			MLPutSymbol(stdlink, "Orange");
			
			MLEndPacket(stdlink);
			MLNextPacket(stdlink);
			MLNewPacket(stdlink);
		//}
		return 1;
	}
	return 0;
}

int FormPrompt(void) {
	write(io[0][1], "\n", 1);
	return 0;
}

void FormRead() {
	const int bytes_at_a_time = 1024;
	char *read_buffer = NULL;
	char *mark;
	int buffer_size = 0;
	int buffer_offset = 0;
	int chars_io;

	while (1) {
		if (buffer_offset + bytes_at_a_time >= buffer_size) {
			buffer_size = bytes_at_a_time + buffer_size;
			read_buffer = realloc(read_buffer, buffer_size);
		}
		chars_io = read(io[1][0], read_buffer + buffer_offset, bytes_at_a_time); 
		if(chars_io>0) buffer_offset += chars_io;
		else if(CheckErrorStatus()>0) {
			MLPutFunction(stdlink, "EvaluatePacket", 1);
			MLPutFunction(stdlink, "Abort", 0);
			return;
		}
		mark=strstr(read_buffer+buffer_offset-15, "#THE-END-MARK#");
		if (mark!=NULL) break;
	}

	*mark = '\0';
	char* p;
	char* cp = read_buffer;
	for(p=read_buffer;*p!='\0';p++) {
		if(*p!='\n' && *p!='\r' && *p!=' ' && *p!='\\') {
			*cp = *p;
			cp++;
		}
	}
	*cp='\0';
	buffer_offset = cp - read_buffer;
	
	MLPutNext(stdlink, MLTKSTR);
	MLPutSize(stdlink, buffer_offset);
	MLPutData(stdlink, read_buffer, buffer_offset);
	free(read_buffer);
}
