#ifndef SYMCONST_HPP
#	define SYMCONST_HPP

#include <stdio.h>

#define		LEN_LABEL	6

#define		SET_SIZE	4

#define		SOL_GREATER		0
#define		SOL_EQUAL		1
#define		SOL_LESS		2
#define		SOL_VALUE		3

#define		TERM_MINUS		0
#define		TERM_ZERO		1
#define		TERM_PLUS		2

typedef char	Label_ [LEN_LABEL + 1];

typedef double *		pFloat_;


class CConstraint;

class CConstraint
{
public:
	static int			m_SymbolCnt;
	static Label_ *		m_pSymbolNames;

	static int			m_EqualCnt;
	static CConstraint *	m_pEqualities;

	char	m_Use;
	char	m_TermSign;
	double	m_Term;
	double *	m_pSet;

	CConstraint ();

	~CConstraint ()
	{
		delete m_pSet;
	}

	int Check ();
	void Copy (CConstraint & p_Solution);
	void Parse (char * p_szLine);
	void Display (FILE * p_pFile);
	void FactorAdd (CConstraint & p_TermSol, int p_Factor);
	int Compare (CConstraint & p_Solution);
	int NeedA ();
	int NeedPlusS ();
	int NeedMinusS ();
};


#endif // SYMCONST_HPP

