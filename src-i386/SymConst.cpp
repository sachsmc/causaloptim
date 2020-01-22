//#include <afx.h>
#include <R.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "MyUtils.h"
#include "SymConst.h"

#define	LINE_LEN	1024

int			CConstraint::m_SymbolCnt;
Label_ *	CConstraint::m_pSymbolNames;

int			CConstraint::m_EqualCnt;
CConstraint *	CConstraint::m_pEqualities;


CConstraint :: CConstraint ()
{
	int		nIdx;

	m_Use = SOL_VALUE;
	m_TermSign = TERM_ZERO;
	m_pSet = new double [m_SymbolCnt];
	memset (m_pSet, 0, m_SymbolCnt * sizeof(double));	// Initialize to 0.
	m_Term = 0.0;
}  /* CConstraint :: CConstraint () */


int CConstraint :: Compare (CConstraint & p_Solution)
{
	if (m_Term != p_Solution.m_Term)
		return 0;

	for (int nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
	{
		if (m_pSet [nSymbol] != p_Solution.m_pSet [nSymbol])
			return 0;
	}

	return 1;
}


void CConstraint :: Copy (CConstraint & p_Solution)
{
	m_Use = p_Solution.m_Use;
	m_TermSign = p_Solution.m_TermSign;
	m_Term = p_Solution.m_Term;
	for (int nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
		m_pSet [nSymbol] = p_Solution.m_pSet [nSymbol];
}  /* CConstraint :: Copy () */


int CConstraint :: Check ()
{
	if (m_Term > 5 ||
		m_Term < -5)
		return 0;

	for (int nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
	{
		if (m_pSet [nSymbol] > 5 ||
			m_pSet [nSymbol] < -5)
			return 0;
	}

	return 1;
}  /* CConstraint :: Check () */


char ParseAdvance (char * &p_pChar)
{
	while (isspace (*p_pChar))
		p_pChar++;
	if (*p_pChar == 0)
		return 0;

	return 1;
}


void CConstraint :: Parse (char * p_szLine)
{
	char *	pChar;
	int		nScan;
	double	Number;
	int		nSymbol;
	char	bLeft = 1;
	char	bMinus;
	char	szBuffer[20];
	char *	pBuf;
	double	Coef;
	double	Sign = 1.0;

//	printf ("Parsing: %s\n", p_szLine);
	m_Term = 0.0;
//	printf ("m_SymbolCnt = %d\n", m_SymbolCnt);
	for (int nCoef = 0; nCoef < m_SymbolCnt; nCoef++)
		 m_pSet [nCoef] = 0.0;

	m_Use = SOL_VALUE;

	pChar = p_szLine;
	while (1)
	{
		bMinus = 0;
		if (!ParseAdvance (pChar))
			break;

//		printf ("pChar = `%s'\n", pChar);
		if (*pChar == '-' ||
			*pChar == '+' ||
			isalnum(*pChar))
		{
			if (*pChar == '-')
			{
				bMinus = 1;
				pChar++;
				ParseAdvance (pChar);
			}
			else if (*pChar == '+')
			{
				pChar++;
				ParseAdvance (pChar);
			}

			if (isdigit(*pChar))
			{
				pBuf = szBuffer;
				while (isdigit(*pChar) ||
					   *pChar == '.')
				{
					*pBuf++ = *pChar++;
				}
				*pBuf = 0;
				sscanf (szBuffer, "%lf", &Coef);
				ParseAdvance (pChar);
			}
			else
				Coef = 1.0;

			if (isalpha (*pChar))
			{
				pBuf = szBuffer;
				while (isalnum(*pChar) || *pChar == '_')
				{
					*pBuf++ = *pChar++;
				}
				*pBuf = 0;

				for (nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
				{
					if (strcmp (szBuffer, m_pSymbolNames [nSymbol]) == 0)
					{
						if (bMinus)
							m_pSet [nSymbol] += -Coef * Sign;
						else
							m_pSet [nSymbol] += Coef * Sign;
						break;
					}
				}
				if (nSymbol >= m_SymbolCnt)
				{
					Rprintf ("ERROR: parsing CConstraint.\n");
					Rprintf ("       szBuffer = `%s'\n", szBuffer);
					Rprintf ("       nSymbol = %d, m_SymbolCnt = %d\n",
							nSymbol, m_SymbolCnt);
				}
			}
			else
			{
				if (bMinus)
					m_Term += -Coef * Sign;
				else
					m_Term += Coef * Sign;
			}

//			printf ("Symbol = %s, bMinus = %d, Coef = %lf, Sign = %lf\n",
//					szBuffer, bMinus, Coef, Sign);
		}
		else if (*pChar == '=' ||
				 *pChar == '<' ||
				 *pChar == '>')
			// Take care of (in)equality signs.
		{
			Sign = -Sign;
			if (*pChar == '=')
				m_Use = SOL_EQUAL;
			else if (*pChar == '<')
			{
				m_Use = SOL_LESS;
				pChar++;
			}
			else
			{
				m_Use = SOL_GREATER;
				pChar++;
			}
			pChar++;
		}

	}

	if (m_Term < 0.0)
		m_TermSign = TERM_MINUS;
	else if (m_Term > 0.0)
		m_TermSign = TERM_PLUS;
	else
		m_TermSign = TERM_ZERO;

//	printf ("Constraint: ");

//	Display (stdout);
}  /* CConstraint :: Parse () */


void CConstraint :: Display (FILE * p_pFile)
{
	int		nSymbol;
	char	bZero = 1;

	switch (m_Use)
	{
	case SOL_EQUAL:
		fprintf (p_pFile, "0 = ");
		break;
	case SOL_GREATER:
		fprintf (p_pFile, "0 <= ");
		break;
	case SOL_LESS:
		fprintf (p_pFile, "0 >= ");
		break;
	case SOL_VALUE:
	default:
		break;
	}
	
	if (m_Term != 0.0)
	{
		bZero = 0;
		fprintf (p_pFile, "%2.0lf", m_Term);
	}

	for (nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
	{	
		if (m_pSet [nSymbol] == 0)
			continue;

		bZero = 0;
		if (m_pSet [nSymbol] < 0)
		{
			if (m_pSet [nSymbol] == -1)
				fprintf (p_pFile, " - %s", m_pSymbolNames [nSymbol]);
			else
				fprintf (p_pFile, " -%lf %s", -m_pSet [nSymbol], m_pSymbolNames [nSymbol]);
		}
		else
		{
			if (m_pSet [nSymbol] == 1)
				fprintf (p_pFile, " + %s", m_pSymbolNames [nSymbol]);
			else
				fprintf (p_pFile, " + %lf %s", m_pSet [nSymbol], m_pSymbolNames [nSymbol]);
		}
	}

	if (bZero)
		fprintf (p_pFile, "%2.0lf", (double) 0.0);

	fprintf (p_pFile, "\n");
	
}  /* CConstraint :: Display () */


void CConstraint :: FactorAdd (CConstraint & p_TermSol, int p_Factor) 
{
	int		nEquality;
	int		nPlus;
	int		nMinus;
	int		nSymbol;
	int		nTerms;
	double	fDiff;
	double	fNewDiff;
	int		Diff;
	int		NewDiff;
	char	bChange;

	m_Term += p_Factor * p_TermSol.m_Term;
	for (nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
		m_pSet [nSymbol] += p_Factor * p_TermSol.m_pSet [nSymbol];

	//*************************************************
	// Now, we must reduce the current solution to its
	// simplest form.

	bChange = 1;
	while (bChange)
	{
		bChange = 0;
		for (nEquality = 0; nEquality < m_EqualCnt; nEquality++)
		{
//			Display (stdout);
			nTerms = 0;
			nPlus = 0;
			nMinus = 0;


			if (m_pEqualities[nEquality].m_Term != 0)
			{
				nTerms++;
				fDiff = fabs (m_Term);
				fNewDiff = fabs (m_Term +
							   m_pEqualities[nEquality].m_Term);
				if (fDiff > fNewDiff)
					nPlus++;

				fNewDiff = fabs (m_Term -
							   m_pEqualities[nEquality].m_Term);
				if (fDiff > fNewDiff)
					nMinus++;
			}

			for (nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
			{
				if (m_pEqualities[nEquality].m_pSet[nSymbol] != 0)
				{
					nTerms++;
					fDiff = fabs (m_pSet [nSymbol]);
					fNewDiff = fabs (m_pSet [nSymbol] +
							 	   m_pEqualities[nEquality].m_pSet [nSymbol]);
					if (fDiff > fNewDiff)
						nPlus++;

					fNewDiff = fabs (m_pSet [nSymbol] -
							 	   m_pEqualities[nEquality].m_pSet [nSymbol]);
					if (fDiff > fNewDiff)
						nMinus++;
				}
			}

//			printf ("FactorAdd: nPlus = %d, nMinus = %d\n", nPlus, nMinus);

			if (nPlus > nTerms - nPlus)
			{
				m_Term += m_pEqualities[nEquality].m_Term;
				for (nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
				{
					m_pSet [nSymbol] += m_pEqualities[nEquality].m_pSet [nSymbol];
				}
				bChange = 1;
			}
			else if (nMinus > nTerms - nMinus)
			{
				m_Term -= m_pEqualities[nEquality].m_Term;
				for (nSymbol = 0; nSymbol < m_SymbolCnt; nSymbol++)
				{
					m_pSet [nSymbol] -= m_pEqualities[nEquality].m_pSet [nSymbol];
				}
				bChange = 1;
			}
		}
	}
}  /* CConstraint :: FactorAdd () */

int CConstraint :: NeedA ()
{
	if (NeedPlusS ())
		return 0;

	return 1;
}  /* CConstraint :: NeedA () */


int CConstraint :: NeedPlusS ()
{
	if (m_TermSign == TERM_PLUS && m_Use == SOL_GREATER ||
		m_TermSign == TERM_ZERO && m_Use == SOL_LESS ||
		m_TermSign == TERM_MINUS && m_Use == SOL_LESS)
		return 1;

	return 0;
}  /* CConstraint :: NeedPlusS () */


int CConstraint :: NeedMinusS ()
{
	if (m_TermSign == TERM_PLUS && m_Use == SOL_LESS ||
		m_TermSign == TERM_ZERO && m_Use == SOL_GREATER ||
		m_TermSign == TERM_MINUS && m_Use == SOL_GREATER)
		return 1;

	return 0;
}  /* CConstraint :: NeedPlusS () */


