
#define MYSTR "toto" 
#define MYSTR2 NULL


const char *foo = NULL;

const char *foo = MYSTR;
const char *foo = MYSTR2;

const char *foo = "blah";
char *foo = "blah";

char *foo = {1,2,3};

const char foo[] = "blah";
char foo[] = "blah";
