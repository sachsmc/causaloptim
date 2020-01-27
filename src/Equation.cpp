#include <windows.h>
#include <stdio.h>
#include <math.h>
#include "CustomFormat.h"
#include "SymbolSet.h"
#include "Equation.h"


CEquation_ :: CEquation_ ()
{
	m_pVariables = NULL;
	m_pParameters = NULL;
	m_pVarCoefs = NULL;
	m_pParamCoefs = NULL;
	m_RelationToZero = RTZ_Value;
	m_ConstantTerm = 0.0;
}  /* CEquation_ :: CEquation_ () */


void CEquation_ :: Initialize 
	(
		CSymbolSet_ * 		p_pVariables, 
		CSymbolSet_ * 		p_pParameters
	)
{
	m_pVariables = p_pVariables;
	m_pParameters = p_pParameters;

	/*
	 * Delete any coefficients if they exist.
	 */
	if (m_pVarCoefs)
		delete [] m_pVarCoefs;
	if (m_pParamCoefs)
		delete [] m_pParamCoefs;

	/*
	 *	Allocate space for and initialize to zero the Variable 
	 *	and Parameter coefficients.
	 */
	m_pVarCoefs = new double [m_pVariables-> Count ()];
	m_pParamCoefs = new double [m_pParameters-> Count ()];

	Zero ();

}  /* CEquation_ :: Initialize () */


CEquation_ :: CEquation_ (CSymbolSet_ * p_pVariables, CSymbolSet_ * p_pParameters)
{
	m_pVariables = NULL;
	m_pParameters = NULL;
	m_pVarCoefs = NULL;
	m_pParamCoefs = NULL;
	m_RelationToZero = RTZ_Value;
	m_ConstantTerm = 0.0;
	Initialize (p_pVariables, p_pParameters);
}  /* CEquation_ :: CEquation_ () */


CEquation_ :: ~CEquation_ ()
{
	if (m_pVarCoefs)
	{
		delete [] m_pVarCoefs;
		m_pVarCoefs = NULL;
	}

	if (m_pParamCoefs)
	{
		delete [] m_pParamCoefs;
		m_pParamCoefs = NULL;
	}
}  /* CEquation_ :: ~CEquation_ () */


void CEquation_ :: Zero ()
{
	if (m_pVarCoefs &&
		m_pParamCoefs)
	{
		memset (m_pVarCoefs, 0, m_pVariables-> Count () * sizeof (double));
		memset (m_pParamCoefs, 0, m_pParameters-> Count () * sizeof (double));
	}
	m_ConstantTerm = 0.0;
	m_RelationToZero = RTZ_Value;
}  /* CEquation_ :: Zero () */


void CEquation_::Negate ()
{
	for (int nVar = 0; nVar < m_pVariables-> Count (); nVar++)
		m_pVarCoefs [nVar] = - m_pVarCoefs [nVar];

	for (int nParam = 0; nParam < m_pParameters-> Count (); nParam++)
		m_pParamCoefs [nParam] = - m_pParamCoefs [nParam];

	m_ConstantTerm = - m_ConstantTerm;

	switch (m_RelationToZero)
	{
	  case RTZ_EqualOrLess:
	  	m_RelationToZero = RTZ_EqualOrGreater;
		break;
	  case RTZ_EqualOrGreater:
	  	m_RelationToZero = RTZ_EqualOrLess;
		break;
	  default:
	  	break;
	}
}  /* CEquation_::Negate () */


WORD CEquation_ :: Parse (char * szString)
{
	char *			pChar;
	int				Sign = 1;
	int				SideCoef = 1;
	double			Coef = 1.0;
	int				ScanCnt;
	Symbol_			Symbol;
	WORD			SymbolID;

	enum 			{	StStartExpression,
						StSign,
						StCoefficient,
						StSymbol
					} State = StStartExpression;

	memset (m_pVarCoefs, 0, m_pVariables-> Count () * sizeof (double));
	memset (m_pParamCoefs, 0, m_pParameters-> Count () * sizeof (double));
	m_ConstantTerm = 0;
	m_RelationToZero = RTZ_Value;

	pChar = szString;

	while (*pChar != '\0')
	{
		while (isspace (*pChar))
			pChar++;
		if (*pChar == '\0')
			break;

		switch (State)
		{
		  case StStartExpression:
		  	Coef = 1.0;
			if (*pChar == '-')
			{
				Sign = -1;
				ScanCnt = 1;
				State = StSign;
			}
			else if (*pChar == '+')
			{
				Sign = 1;
				ScanCnt = 1;
				State = StSign;
			}
			else if (isdigit(*pChar) ||
					 *pChar == '.' &&
					 isdigit(pChar[1]))
			{
				sscanf (pChar, "%lf%n", &Coef, &ScanCnt);
				State = StCoefficient;
			}
			else if (isalpha(*pChar))
			{
				ScanCnt = ParseSymbol (pChar, Symbol);
				if (ScanCnt == 0)
					return FALSE;
				State = StSymbol;
				SymbolID = m_pVariables-> GetID (Symbol);
				if (SymbolID != INVALID_SYMBOL_ID)
				{
					m_pVarCoefs [SymbolID] += SideCoef * Sign * Coef;
					break;
				}
				SymbolID = m_pParameters-> GetID (Symbol);
				if (SymbolID != INVALID_SYMBOL_ID)
				{
					m_pParamCoefs [SymbolID] += SideCoef * Sign * Coef;
					break;
				}
				return FALSE;
			}
			else
				return FALSE;
			break;

		  case StSign:
		  	Coef = 1.0;
			if (isdigit(*pChar) ||
					 *pChar == '.' &&
					 isdigit(pChar[1]))
			{
				sscanf (pChar, "%lf%n", &Coef, &ScanCnt);
				State = StCoefficient;
			}
			else if (isalpha(*pChar))
			{
				ScanCnt = ParseSymbol (pChar, Symbol);
				if (ScanCnt == 0)
					return FALSE;
				State = StSymbol;

				SymbolID = m_pVariables-> GetID (Symbol);
				if (SymbolID != INVALID_SYMBOL_ID)
				{
					m_pVarCoefs [SymbolID] += SideCoef * Sign * Coef;
					break;
				}
				SymbolID = m_pParameters-> GetID (Symbol);
				if (SymbolID != INVALID_SYMBOL_ID)
				{
					m_pParamCoefs [SymbolID] += SideCoef * Sign * Coef;
					break;
				}
				return FALSE;
			}
			else
				return FALSE;
			break;

		  case StCoefficient:
			if (isalpha(*pChar))
			{
				ScanCnt = ParseSymbol (pChar, Symbol);
				if (ScanCnt == 0)
					return FALSE;
				State = StSymbol;

				/*
				 *	Add the appropriate value to the variable
				 *	or parameter's coefficient.
				 */
				SymbolID = m_pVariables-> GetID (Symbol);
				if (SymbolID != INVALID_SYMBOL_ID)
				{
					m_pVarCoefs [SymbolID] += SideCoef * Sign * Coef;
					break;
				}
				SymbolID = m_pParameters-> GetID (Symbol);
				if (SymbolID != INVALID_SYMBOL_ID)
				{
					m_pParamCoefs [SymbolID] += SideCoef * Sign * Coef;
					break;
				}
				return FALSE;
			}
			else if (*pChar == '-')
			{
				m_ConstantTerm += SideCoef * Sign * Coef;
				Sign = -1;
				ScanCnt = 1;
				State = StSign;
			}
			else if (*pChar == '+')
			{
				m_ConstantTerm += SideCoef * Sign * Coef;
				Sign = 1;
				ScanCnt = 1;
				State = StSign;
			}
			else if (*pChar == '=' ||
					 *pChar == '<' && *(pChar+1) == '=' ||
					 *pChar == '>' && *(pChar+1) == '=')
			{
				// Check that no relation has already been parsed.
				if (m_RelationToZero != RTZ_Value)
					return FALSE;

				m_ConstantTerm += SideCoef * Sign * Coef;

				switch (*pChar)
				{
				  case '=':
				  	m_RelationToZero = RTZ_Equal;
					ScanCnt = 1;
					break;

				  case '<':
				  	m_RelationToZero = RTZ_EqualOrLess;
					ScanCnt = 2;
					break;

				  case '>':
				  	m_RelationToZero = RTZ_EqualOrGreater;
					ScanCnt = 2;
					break;

				  default:
				  	break;
				}
				
				Sign = 1;
				SideCoef = -1;
				State = StStartExpression;
			}
			else
				return FALSE;
			break;

		  case StSymbol:	// A symbol was just parsed.  Expecting a relation operator,
		  					//	plus, or minus.
		  	Coef = 1.0;
			if (*pChar == '-')
			{
				Sign = -1;
				ScanCnt = 1;
				State = StSign;
			}
			else if (*pChar == '+')
			{
				Sign = 1;
				ScanCnt = 1;
				State = StSign;
			}
			else if (*pChar == '=' ||
					 *pChar == '<' && *(pChar+1) == '=' ||
					 *pChar == '>' && *(pChar+1) == '=')
			{
				// Check that no relation has already been parsed.
				if (m_RelationToZero != RTZ_Value)
					return FALSE;

				switch (*pChar)
				{
				  case '=':
				  	m_RelationToZero = RTZ_Equal;
					ScanCnt = 1;
					break;

				  case '<':
				  	m_RelationToZero = RTZ_EqualOrLess;
					ScanCnt = 2;
					break;

				  case '>':
				  	m_RelationToZero = RTZ_EqualOrGreater;
					ScanCnt = 2;
					break;

				  default:
				  	break;
				}
				
				Sign = 1;
				SideCoef = -1;
				State = StStartExpression;
			}
			else
				return FALSE;
		  	break;

		  default:
		  	return FALSE;
		}

		pChar += ScanCnt;
	}
	
	/*
	 *	The end of the parse string was reached; perform
	 *	any unfinished processing.
	 */
	switch (State)
	{
	  case StStartExpression:
	  case StSign:
	  	return FALSE;

	  case StCoefficient:
		m_ConstantTerm += SideCoef * Sign * Coef;
	  	return TRUE;

	  case StSymbol:
	  	return TRUE;

	  default:
	  	return FALSE;
	}
}  /* CEquation_ :: Parse () */


char * CEquation_ :: BuildOutput 
	(
		char * 		p_szString, 
		WORD 		p_Decimal,
		BOOL		p_bSeperate
	)
{
	BOOL		bFirstCoef = TRUE;
					// Specifies whether the current term is the first
					//	term to be formatted in the string.

	double		Coef;
					// Coefficient of first term.

	char		szSign [3];
					// Term operation ('+', '-', or '' [first term '+']).

	char		szReal [20];
					// Formatted absolute value for current term.

	char		szTerm [50];
					// Current formatted term.

	char		szSeperator [2];
					// Spacing between every token in output string.

	char		szTermSep [2] = "";
					// Inter-term spacing (empty for first term).

	WORD		nSymbol;
					// Index into a set of symbols (variables and parameters).

	double		Leeway;
					// Difference between two coefficients at which they are 
					//	considered equal.

	/*
	 * Clear the output string.
	 */
	*p_szString = '\0';

	/*
	 * Determine the leeway between coefficients.
	 */
	Leeway = 0.5 * pow (10.0,-p_Decimal);

	/*
	 * Set the seperator character.
	 */
	if (p_bSeperate)
		strcpy (szSeperator, " ");	
	else
		strcpy (szSeperator, "");

	/******************************************
	 * Format all terms for the variables.
	 *****************************************/
	for (nSymbol = 0; nSymbol < m_pVariables-> Count (); nSymbol++)
	{
		Coef = m_pVarCoefs [nSymbol];

		/*
		 * Skip any coefficients that are insignificant.
		 */
		if (Approx(Coef,0.0,Leeway))
			continue; 

		/*
		 * Determine the sign to display for the current term.
		 */
		if (Coef < 0.0)
		{
			sprintf (szSign, "-%s", szSeperator);
			Coef = -Coef;
		}
		else
		{
			if (bFirstCoef)
				strcpy (szSign, "");
			else
				sprintf (szSign, "+%s", szSeperator);
		}

		/*
		 * Format the absolute value of the coefficient.
		 */
		FormatReal (szReal, Coef, 1, p_Decimal, FALSE);

		/*
		 * Format the entire term, and concatenate it to the output string.
		 */
		if (Approx(Coef,1.0,Leeway) ||
			Approx(Coef,-1.0,Leeway))
			sprintf (szTerm, "%s%s%s",
					 szTermSep,
					 szSign,
					 m_pVariables-> GetName (nSymbol));
		else
			sprintf (szTerm, "%s%s%s%s%s",
					 szTermSep,
					 szSign,
					 szReal,
					 szSeperator,
					 m_pVariables-> GetName (nSymbol));
 		strcat (p_szString, szTerm);

		/*
		 * Following terms are not the first.
		 */
		bFirstCoef = FALSE;
		strcpy (szTermSep, szSeperator);
	}

	/*****************************************
	 * Format all terms for the parameters.
	 ****************************************/
	for (nSymbol = 0; nSymbol < m_pParameters-> Count (); nSymbol++)
	{
		Coef = m_pParamCoefs [nSymbol];

		/*
		 * Skip any coefficients that are insignificant.
		 */
		if (Approx(Coef,0.0,Leeway))
			continue; 

		/*
		 * Determine the sign to display for the current term.
		 */
		if (Coef < 0.0)
		{
			sprintf (szSign, "-%s", szSeperator);
			Coef = -Coef;
		}
		else
		{
			if (bFirstCoef)
				strcpy (szSign, "");
			else
				sprintf (szSign, "+%s", szSeperator);
		}

		/*
		 * Format the absolute value of the coefficient.
		 */
		FormatReal (szReal, Coef, 1, p_Decimal, FALSE);

		/*
		 * Format the entire term, and concatenate it to the output string.
		 */
		if (Approx(Coef,1.0,Leeway) ||
			Approx(Coef,-1.0,Leeway))
			sprintf (szTerm, "%s%s%s",
					 szTermSep,
					 szSign,
					 m_pParameters-> GetName (nSymbol));
		else
			sprintf (szTerm, "%s%s%s%s%s",
					 szTermSep,
					 szSign,
					 szReal,
					 szSeperator,
					 m_pParameters-> GetName (nSymbol));
		strcat (p_szString, szTerm);

		/*
		 * Following terms are not the first.
		 */
		bFirstCoef = FALSE;
		strcpy (szTermSep, szSeperator);
	}

	/******************************************
	 * Format the constant term.
	 *****************************************/
	Coef = m_ConstantTerm;

	/*
	 * Skip any coefficients that are insignificant.
	 */
	if (! Approx(Coef,0.0,Leeway) ||
		bFirstCoef)
	{
		/*
		 * Determine the sign to display for the current term.
		 */
		if (Coef < 0.0)
		{
			sprintf (szSign, "-%s", szSeperator);
			Coef = -Coef;
		}
		else
		{
			if (bFirstCoef)
				strcpy (szSign, "");
			else
				sprintf (szSign, "+%s", szSeperator);
		}

		/*
		 * Format the absolute value of the coefficient.
		 */
		FormatReal (szReal, Coef, 1, p_Decimal, FALSE);

		/*
		 * Format the entire term, and concatenate it to the output string.
		 */
		sprintf (szTerm, "%s%s%s",
				 szTermSep,
				 szSign,
				 szReal);
		strcat (p_szString, szTerm);
	}

	/*
	 * Append the expression's relation to zero, if any.
	 */
	switch (m_RelationToZero)
	{
	  case RTZ_Equal:
	  	sprintf (szTerm, "%s=%s0", szTermSep, szTermSep);
		break;

	  case RTZ_EqualOrGreater:
	  	sprintf (szTerm, "%s>=%s0", szTermSep, szTermSep);
	  	break;

	  case RTZ_EqualOrLess:
	  	sprintf (szTerm, "%s<=%s0", szTermSep, szTermSep);
		break;

	  default:
	  	*szTerm = '\0';
	  	break;
	}
	strcat (p_szString, szTerm);

	// Return address of formatted string.
	return p_szString;
}  /* CEquation_ :: BuildOutput () */



BOOL CEquation_ :: Copy (CEquation_ * p_pEquation)
{
	if (m_pVariables != p_pEquation-> m_pVariables ||
		m_pParameters != p_pEquation-> m_pParameters)
	{
		return FALSE;
	}

	m_RelationToZero = p_pEquation-> m_RelationToZero;

	memcpy (m_pVarCoefs, p_pEquation-> m_pVarCoefs,
			m_pVariables-> Count () * sizeof(double));
	memcpy (m_pParamCoefs, p_pEquation-> m_pParamCoefs,
			m_pParameters-> Count () * sizeof(double));
	m_ConstantTerm = p_pEquation-> m_ConstantTerm;

	return TRUE;
}  /* CEquation_ :: Copy () */


ProjectResult_ CEquation_ :: Projection (CEquation_ * p_pSrcEquation)
{
	WORD		nSrcSymbol;
				// Index into source equation's symbol sets.

	WORD		nTgtSymbol;
				// Index into this equation's symbol sets.

	ProjectResult_	ProjectResult;
					// Indicates whether data loss occurred during this 
					// projection.

	ProjectResult = ProjectNoDataLoss;

	Zero ();
	for (nSrcSymbol = 0; nSrcSymbol < p_pSrcEquation-> m_pVariables-> Count (); nSrcSymbol++)
	{
		nTgtSymbol = m_pVariables-> GetID (
				p_pSrcEquation-> m_pVariables-> GetName (nSrcSymbol));
		if (nTgtSymbol == INVALID_SYMBOL_ID)
		{
			if (! Approx (p_pSrcEquation-> m_pVarCoefs [nSrcSymbol], 
					0.0, LEEWAY))
				ProjectResult = ProjectDataLoss;
		}
		else
			m_pVarCoefs [nTgtSymbol] = 
					p_pSrcEquation-> m_pVarCoefs [nSrcSymbol];
	}

	for (nSrcSymbol = 0; nSrcSymbol < p_pSrcEquation-> m_pParameters-> Count (); nSrcSymbol++)
	{
		nTgtSymbol = m_pParameters-> GetID (
				p_pSrcEquation-> m_pParameters-> GetName (nSrcSymbol));
		if (nTgtSymbol == INVALID_SYMBOL_ID)
		{
			if (! Approx (p_pSrcEquation-> m_pParamCoefs [nSrcSymbol],
					0.0, LEEWAY))
				ProjectResult = ProjectDataLoss;
		}
		else
			m_pParamCoefs [nTgtSymbol] = 
					p_pSrcEquation-> m_pParamCoefs [nSrcSymbol];
	}

	m_ConstantTerm = p_pSrcEquation-> m_ConstantTerm;
	m_RelationToZero = p_pSrcEquation-> m_RelationToZero;

	return ProjectResult;
}  /* CEquation_ :: Projection () */


BOOL CEquation_ :: FactorAdd (CEquation_ * p_pEquation, double p_Factor)
{
	WORD		nSymbol;
				// Index into the symbol sets.

	/*
	 * This operation is only valid if the two equations are both
	 *	values, or both equal to zero.
	 */
	if (m_pVariables != p_pEquation-> m_pVariables ||
		m_pParameters != p_pEquation-> m_pParameters ||
		! (p_pEquation-> m_RelationToZero == RTZ_Value ||
		   p_pEquation-> m_RelationToZero == RTZ_Equal))
	{
		return FALSE;
	}

	m_ConstantTerm += p_Factor * p_pEquation-> m_ConstantTerm;

	for (nSymbol = 0; nSymbol < m_pVariables-> Count (); nSymbol++)
	{
		m_pVarCoefs [nSymbol] += p_Factor * 
								 p_pEquation-> m_pVarCoefs [nSymbol];
	}

	for (nSymbol = 0; nSymbol < m_pParameters-> Count (); nSymbol++)
	{
		m_pParamCoefs [nSymbol] += p_Factor * 
								 p_pEquation-> m_pParamCoefs [nSymbol];
	}

	return TRUE;
}  /* CEquation_ :: FactorAdd () */


BOOL CEquation_ :: Divide (double p_Divisor)
{
	WORD		nSymbol;
				// Index into variable or parameter symbol sets.

	if (Approx (p_Divisor, 0.0, LEEWAY))
		return FALSE;

	for (nSymbol = 0; nSymbol < m_pVariables-> Count (); nSymbol++)
		m_pVarCoefs [nSymbol] /= p_Divisor;

	for (nSymbol = 0; nSymbol < m_pParameters-> Count (); nSymbol++)
		m_pParamCoefs [nSymbol] /= p_Divisor;

	m_ConstantTerm /= p_Divisor;

	return TRUE;
}  /* CEquation_ :: Divide () */


BOOL CEquation_ :: ForceForm (RelationToZero_ p_RelToZero)
{
	WORD		nSymbol;
				// Index into variable or parameter symbol sets.

	if (p_RelToZero == m_RelationToZero)
		return TRUE;
	if (p_RelToZero != RTZ_EqualOrGreater &&
			p_RelToZero != RTZ_EqualOrLess)
		return FALSE;
	if (m_RelationToZero != RTZ_EqualOrGreater &&
			m_RelationToZero != RTZ_EqualOrLess)
		return FALSE;

	/*
	 * The relation is being swapped; negate all coefficients.
	 */
	for (nSymbol = 0; nSymbol < m_pVariables-> Count (); nSymbol++)
		m_pVarCoefs [nSymbol] = - m_pVarCoefs [nSymbol];

	for (nSymbol = 0; nSymbol < m_pParameters-> Count (); nSymbol++)
		m_pParamCoefs [nSymbol] = - m_pParamCoefs [nSymbol];

	m_ConstantTerm = - m_ConstantTerm;

	m_RelationToZero = p_RelToZero;
	return TRUE;

}  /* CEquation_ :: ForceForm () */




