#ifndef CUSTOM_FORMAT_H
#define	CUSTOM_FORMAT_H

#include <windows.h>

#define	LEEWAY			((double) 0.000001)
			// Two coefficients different by less than this amount
			//	will be considered equal.

#define Approx(a,b,c)		((a) < (b)+(c) && (a) > (b)-(c))
			// Returns TRUE if 'a' is within 'c' units of 'b'.

char * FormatReal 
	(
		char *		p_szTarget,	// String into which the value is to be 
								//	formatted.
		double 		p_Real,		// Real number to format. 
		WORD 		p_Width,	// Maximum width of field. 
		WORD 		p_Decimal,	// Maximum number of decimal places to show. 
		BOOL 		p_bPadRight	// TRUE: insignificant decimal places up to the
								//	field's width will be filled with space.
	);

#endif // CUSTOM_DISPLAY_H
