//#include <afx.h>
#include <windows.h>
#include <stdio.h>
#include "CustomFormat.h"

char * FormatReal 
	(
		char *		p_szTarget,	// String into which the value is to be 
								//	formatted.
		double 		p_Real,		// Real number to format. 
		WORD 		p_Width,	// Maximum width of field.  The lhs of the
								//	decimal may overflow this width to the 
								//	left. 
		WORD 		p_Decimal,	// Maximum number of decimal places to show. 
		BOOL 		p_bPadRight	// TRUE: insignificant decimal places up to the
								//	field's width will be filled with space.
	)
{
	char		szFormat [20];
					// Initial formatting (printf scan codes).

	char *		pChar;
					// Reference into the target string.

	char		PadChar;
					// Character to replace trailing zeroes with.

	snprintf (szFormat, 20, "%%%d.%dlf", p_Width, p_Decimal);
	snprintf (p_szTarget, 256, szFormat, p_Real);

	if (p_Decimal != 0)		// There are trailing decimal places.
	{
		if (p_bPadRight)
			PadChar = ' ';
		else
			PadChar = '\0';

		/*
		 * Replace all trailing zeroes with the pad character.
		 */
		pChar = p_szTarget + strlen (p_szTarget) - 1;
		while (*pChar == '0' &&
			   pChar > p_szTarget)
			*pChar-- = PadChar;

		/*
		 * If the right-most digit is now a decimal point, replace it.
		 */
		if (*pChar == '.')
			*pChar = PadChar;
	}
	return p_szTarget;
}  /* FormatReal () */

