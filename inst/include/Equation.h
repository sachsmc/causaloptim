#ifndef EQUATION_H
#define	EQUATION_H

#include <windows.h>
#include "SymbolSet.h"

enum RelationToZero_
{
	RTZ_Value,			// No relationship holds (just an expression).
	RTZ_Equal,			// Expression is equal to zero.
	RTZ_EqualOrGreater,	// Expression is greater than or equal to zero.
	RTZ_EqualOrLess		// Expression is less than or equal to zero.
};

enum ProjectResult_
{
	ProjectDataLoss,	// Not all non-zero coefficients were projected.
	ProjectNoDataLoss	// All non-zero coefficients were projected.
};

class CEquation_
{

public:

	RelationToZero_	m_RelationToZero;
					// Indicates if the expression is greater than, 
					//	less than, or equal to zero.

	CSymbolSet_ *	m_pVariables;
					// Reference to the set of variable symbols
					//	usable by this equation.

	CSymbolSet_ *	m_pParameters;
					// Reference to the set of parameter symbols
					//	usable by this equation.

	double *		m_pVarCoefs;
					// Coefficients of variables in the equation; indexed
					//	parallel to the symbols in m_pVariables.

	double *		m_pParamCoefs;
					// Coefficients of parameters in the equation; indexed
					//	parallel to the symbols in m_pParameters.

	double			m_ConstantTerm;
					// Constant term added into the equation.

	CEquation_ (CSymbolSet_ * p_pVariables, CSymbolSet_ * p_pParameters);

	CEquation_ ();

	~CEquation_ ();

	void Negate ();

	void Initialize (CSymbolSet_ * p_pVariables, CSymbolSet_ * p_pParameters);

	void Zero ();

	WORD Parse (char * szString);

	char * BuildOutput (char * szString, WORD p_Decimal, BOOL p_bSeperate);

	BOOL Copy (CEquation_ * p_pEquation);

	ProjectResult_ Projection (CEquation_ * p_pEquation);

	BOOL FactorAdd (CEquation_ * p_pEquation, double p_Factor);

	BOOL /* CEquation_ :: */ Divide (double p_Divisor);

	BOOL ForceForm (RelationToZero_ p_RelToZero);

};  /* class CEquation_ */


#endif // EQUATION_H
