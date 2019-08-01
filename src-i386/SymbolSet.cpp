//#include <afx.h>
#include <windows.h>
#include "SymbolSet.h"


CSymbolSet_ :: CSymbolSet_
	(
		WORD		p_Count
	)
{
	m_Count = p_Count;

	/*
	 *	Allocate and initialize the symbols.
	 */
	m_pSymbols = NULL;
	m_pSymbols = new Symbol_ [m_Count];
	if (m_pSymbols)
		memset (m_pSymbols, 0, m_Count * sizeof (Symbol_));
}  /* CSymbolSet_ :: CSymbolSet_ () */


CSymbolSet_ :: ~CSymbolSet_ ()
{
	if (m_pSymbols)
		delete [] m_pSymbols;
}  /* CSymbolSet_ :: ~CSymbolSet_ () */


char * CSymbolSet_ :: GetName 
	(
		WORD 	p_SymbolID
	)
{
	return m_pSymbols [p_SymbolID];
}  /* CSymbolSet_ :: GetName () */


WORD CSymbolSet_ :: GetID 
	(
		char * 	p_szNameSource
	)
{
	WORD			nID;

	for (nID = 0; nID < m_Count; nID++)
	{
		if (strcmp (m_pSymbols [nID], p_szNameSource) == 0)
			return nID;
	}
	return INVALID_SYMBOL_ID;
}  /* CSymbolSet_ :: GetID () */


void CSymbolSet_ :: Assign (WORD p_SymbolID, Symbol_ p_Symbol)
{
	if (p_SymbolID < m_Count)
		strncpy (m_pSymbols [p_SymbolID], p_Symbol, MAX_SYMBOL_LEN);
}  /* CSymbolSet_ :: Assign () */


WORD ParseSymbol
	(
		char *		p_szString,
		Symbol_ &	p_Symbol
	)
/*
 *	RETURNS
 *		Number of characters scanned during parsing.
 *			0, if a symbol name was not successfully parsed.
 */
{
	WORD		nScan = 0;
	char *		pSrcChar = p_szString;
	char *		pTgtChar = p_Symbol;

	/*
	 *	Skip any leading space characters.
	 */
	while (isspace (*pSrcChar))
		pSrcChar++;

	/*
	 *	First character must be in the alphabet.
	 */
	if (!isalpha (*pSrcChar))
		return 0;

	/*
	 *	Copy all immediately following alphanumeric characters
	 *	or underscores.
	 */
	while (__iscsym (*pSrcChar))
	{
		*pTgtChar++ = *pSrcChar++;
		nScan++;
		if (nScan > MAX_SYMBOL_LEN)
		{
			/*
			 *	Symbol name exceeded maximum length.
			 */
			p_Symbol [0] = NULL;
			return 0;
		}
	}
	*pTgtChar = NULL;
	return nScan;
}  /* ParseSymbol () */


