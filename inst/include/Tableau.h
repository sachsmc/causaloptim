#ifndef TABLEAU_HPP
#	define TABLEAU_HPP

#include <windows.h>
#include <stdio.h>
#include <string>
#include "SymConst.h"


#define		LEN_STRING		1024

#define		MAX_VERTICES	1000

typedef char String_ [LEN_STRING + 1];


class CEnumRcd
{
	double			m_Value;
	WORD *			m_pNonBasics;

  public:

	CEnumRcd *		m_pNext;

	CEnumRcd (double p_Value, WORD * p_pNonBasics)
	{
		m_pNext = NULL;
		m_Value = p_Value;
		m_pNonBasics = p_pNonBasics;
	}
	WORD * Contents ()
	{
		return m_pNonBasics;
	}
	double Value ()
	{
		return m_Value;
	}
};  /* class CEnumRcd */



class CTableau
{
		//*******************************************
		// Refer to
		//	Linear programming and Extensions
		//	by George B. Dantzig
		//	Princeton University Press
		//	Princeton, NJ
		//	1963
		//*******************************************
		// NOTE:
		//	The variable corresponding to the (-w) and (-z) will
		//	always be the first and second elements, respectively, in the 
		//	variable and basis arrays.
		//*******************************************

	CEnumRcd *		m_pEnumList;
	CEnumRcd *		m_pEnumCrnt;
	int				m_EnumListLen;

	int				m_VarCnt;		// Number of variables, including slack
									//	and augmenters (includes -z).
	Label_ *		m_pVarLabels;	// Names of all variables 
									//	(includes -z).

	int				m_AugCnt;		// Number of augmented variables needed for
									// negative solution values.

	int				m_ParamCnt;		// Number of user defined parameters.
	Label_ *		m_pParamNames;	// Names of all user defined parameters.

	int				m_BasisCnt;		// Size of basis set (includes -z).

	int				m_SupplCnt;		// For Si's with negative one coefficient
									//	(these also need to be augmented).

	int				m_ConstCnt;		// Number of user-defined constraints.
	CConstraint *	m_pConstraints;	// User-defined constraints.

	CConstraint *	m_pObjective;	// Specification of objective function
									//	(this really isn't a constraint, but I
									//	am using the constraint to hold the 
									//	coefficients of the objective function)

	double *		m_pOrigSolution;// Solution vectors for the basis 
	double *		m_pSolution;	//	(includes -w and -z).
	
	pFloat_ *		m_pOrigTable;	// Simplex Tableau (now, also maintains the
	pFloat_ * 		m_pTable;		//	coefficients of the objective function)

	double *		m_pCj;			// Cost factors in objective function.

	int *			m_pOrigBasisVars;// Maps the index of the basis to
	int *			m_pBasisVars;	//	its corresponding variable.

	int				m_VertexCnt;	// Number of vertices enumerated so far.
	pFloat_			m_pVertices[MAX_VERTICES];	// Enumerated vertices.

	char *			m_pSlackFlag;	// Boolean vector indicating which slack
									//	variables have been flagged.  See
									//	step 2 of Mattheiss' algorithm.

	
public:
    
	CTableau (	int 		p_ParamCnt,
				int			p_Rows,
				Label_		p_pParamNames[],
				pFloat_ *	p_pA,
				double *	p_pb);

	void Setup (int 		p_ParamCnt,
				int			p_Rows,
				Label_		p_pParamNames[],
				pFloat_ *	p_pA,
				double *	p_pb);

    CTableau (FILE * p_pFile);
//	CTableau (int p_VarCnt, int p_BasisCnt, 
//			  Label_ * p_pVarLabels, int * p_pBasisVars);
	~CTableau ();

	void SetSolutions (double * p_pSolutions);
	void SetCj (double * p_pFloats);
	void SetConstraint (int p_nBasis, double * p_pCoefs);
	void Reset ();

	void DetermineSwap (int & p_Basis, int & p_Var);
	void TradeBasis (int p_nBasis, int p_nVar);
	std::string DecisionDisplay ();
//	void EvaluateNet ();
	double GetSolution (char * p_szVarName);
	void SetSolution (int p_nBasis, double p_Solution);
	void SetConstraintTerm (int p_nBasis, double p_Term);
	int SetCoefficient (int p_nBasis, char * p_szVar, double p_Coef);
	int CheckConstraints ();
	double ObjectiveValue ();
	char Optimize ();
	void DisplayBasis ();
	void DisplayParams ();
	void DropVars ();
	void WorkToOrig ();
	void AddUnique (double p_Value, WORD * p_pNonBasics);
	WORD * PopFirst ();
	std::string VertexEnumerate ();
	void GenerateTableau (WORD * p_pNonBasics);
	void DivideRow (int p_Basis, double p_Divisor);
	void FactorAddRows (int p_SrcRow, int p_TgtRow, double p_Factor);
	void AddEnumRcd (int * p_pBasisVars, double p_Value);
	void DisplayEnumRcds ();
	void AddVertex (int * p_pBasisVars, double * p_pSolution);
	std::string DisplayVertices ();
	BOOL  GetVertex (int p_nVertex, double * p_pVertex, int p_VertexLength);
	int	VertexCount ()
	{
		return m_VertexCnt;
	}
};  /* class CTableau */


#endif

