#include "before.h"
#include <linux/foo.h>
#include <asm/semaphore.h>
#include <linux/foo2.h>
#include "after.h"
#ifdef FOO
#include <linux/bar.h>
#endif FOO
