#ifndef Windows_H
#define	Windows_H

#include <stddef.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>

//#define	NULL				0
#define	TRUE				1
#define	FALSE				0

#define __iscsym(a)		(isalnum(a) || a=='_')

typedef unsigned short		WORD;
typedef unsigned int		DWORD;
typedef unsigned char		BYTE;
typedef unsigned int		BOOL;

#endif // Windows_H

