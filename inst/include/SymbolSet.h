#ifndef SYMBOL_SET_H
#define SYMBOL_SET_H

#include <windows.h>

#define	MAX_SYMBOL_LEN		20
			// Maximum name length of a symbol.

#define	INVALID_SYMBOL_ID	((WORD) 0xFFFF)

typedef char  	Symbol_ [MAX_SYMBOL_LEN + 1];
					// NULL terminated symbol name.

class CSymbolSet_
/*
 *	Maintains an enumerated set of symbol names.
 */
{
	WORD		m_Count;
				// Number of symbols in the set.

	Symbol_ *	m_pSymbols;
				// Symbol Names. 
	
public:
	CSymbolSet_ (WORD p_Count);

	~CSymbolSet_ ();

	char * GetName (WORD p_SymbolID);

	WORD GetID (char * p_szNameSource);

	void Assign (WORD p_SymbolID, Symbol_ p_Symbol);

	WORD Count ()
	{
		return m_Count;
	}
};  /* class CSymbolSet_ */


WORD ParseSymbol (char * p_szString, Symbol_ & p_Symbol);

#endif // SYMBOL_SET_H
