#ifndef EQUATION_SET_H
#define	EQUATION_SET_H

#include <windows.h>
#include "SymbolSet.h"
#include "Equation.h"


class CEquationSet_
{

public:

	WORD			m_Count;
					// Number of equations in the set.

	CSymbolSet_ *	m_pVariables;
					// Set of variable symbols.

	CSymbolSet_ *	m_pParameters;
					// Set of parameter symbols.

	CEquation_ *	m_pEquations;
					// Array of equations in the set.

	CEquationSet_ (CSymbolSet_ * p_pVariables, CSymbolSet_ * p_pParameters, 
				   WORD p_EqnCount);

	~CEquationSet_ ();

	CEquationSet_ * Duplicate ();

/*****************
	BOOL GaussianElimination ();

	CSymbolSet_ * EliminateVariables (CEquationSet_ * p_pModifyEquations);
			// This function assumes an implicit constraint that all variables
			//	are greater than or equal to zero.  Therefore, for each
			//	variable eliminated, a new inequality will be introduced.
			// In addition, any redundant equalities will be eliminated from
			//	the new set of equations through Gaussian elimination.
			// The Modify Equations will also be updated by eliminating variables.
			// Returns the new reduced symbol set of variables.
******************/

};  /* class CEquationSet_ */



#endif // EQUATION_SET
