//#include <afx.h>
#include <windows.h>
#include <stdio.h>
#include <math.h>
#include "CustomFormat.h"
#include "SymbolSet.h"
#include "Equation.h"
#include "EquationSet.h"

#define	VAR_NOT_ELIMINATED		((WORD) 0xFFFF)
#define	VAR_ELIMINATED			((WORD) 0xFFFE)




CEquationSet_ :: CEquationSet_ 
	(
		CSymbolSet_ * 	p_pVariables, 
		CSymbolSet_ * 	p_pParameters,
		WORD			p_EqnCount
	)
{
	WORD		nEqn;

	m_Count = p_EqnCount;
	m_pVariables = p_pVariables;
	m_pParameters = p_pParameters;

	m_pEquations = new CEquation_ [m_Count];
	for (nEqn = 0; nEqn < m_Count; nEqn++)
		m_pEquations [nEqn]. Initialize (m_pVariables, m_pParameters);
}  /* CEquationSet_ :: CEquationSet_ () */


CEquationSet_ :: ~CEquationSet_ ()
{
	if (m_pEquations)
	{
		delete [] m_pEquations;
		m_pEquations = NULL;
	}
}  /* CEquationSet_ :: ~CEquationSet_ () */


CEquationSet_ * CEquationSet_ :: Duplicate ()
{
	CEquationSet_ *		pEquationSet;
	WORD				nEqn;

	pEquationSet = new CEquationSet_ (m_pVariables,
									  m_pParameters,
									  m_Count);

	for (nEqn = 0; nEqn < m_Count; nEqn++)
		pEquationSet-> m_pEquations [nEqn]. Copy (& m_pEquations [nEqn]);

	return pEquationSet;
}  /* CEquationSet_ :: Duplicate () */


#ifdef JUNK

CSymbolSet_ * CEquationSet_ :: EliminateVariables ()
{
	WORD			nEqn;
					// Index of the current equation set.

	WORD			EqualityCnt;
					// Number of equations of type equality;

	WORD			nEquality;
					// Index into the equality equation set.

	WORD			nInequality;
					// Index into the inequality equation set.

	WORD			nRemainingVar;
					// Index into the remaining-variable symbol set.

	WORD			nVar;
					// Index into the variable symbol set.

	WORD			Rank;
					// Rank of the set of equalities.

	WORD *			pElimVarToEquality;
					// Array that maps eliminated variables to the
					// equality used to substitute out that
					// variable.  Allocated.
					// A value of VAR_NOT_ELIMINATED if a variable
					// is not to be eliminated.

	CSymbolSet_ *	pRemainingVariables;
					// Set of all variables not eliminated.
					// Allocated.

	CEquationSet_ *	pEqualitySet;
					// Equation set consisting of only equalities.
					// Allocated.

	CEquationSet_ *	pInequalitySet;
					// Final equation set that contains only
					// inequalities.  Allocated.

	CEquation_ *	pWorkEqn;
					// Copy of original equation for performing
					// linear operations.  Allocated.

	CEquation_ *	pEquality;
					// Reference to equation within the equality set.

	CEquation_ *	pInequality;
					// Reference to equation within the inequality set.

	CEquation_ *	pEquation;
					// Reference to equation within the original equation set.

	/****************************************************
	 * Create a set of equations consisting only of those
	 * equations in the current set that are equalities.
	 ***************************************************/

	/*
	 * Determine how many equalities exist.
	 */
	EqualityCnt = 0;
	for (nEqn = 0; nEqn < m_Count; nEqn++)
	{
		pEquation = & m_pEquations [nEqn];
		if (pEquation-> m_RelationToZero == RTZ_Equal)
			EqualityCnt++;
	}

	/*
	 * Create a new equation set consisting of the equality
	 * equations.
	 */
	pEqualitySet = new CEquationSet_ (m_pVariables,
									  m_pParameters,
									  EqualityCnt);
	nEquality = 0;
 	for (nEqn = 0; nEqn < m_Count; nEqn++)
	{
		pEquation = & m_pEquations [nEqn];
		if (pEquation-> m_RelationToZero == RTZ_Equal)
		{
			pEquality = & pEqualitySet-> m_pEquations [nEquality];
			pEquality-> Copy (pEquation);
			nEquality++;
		}
	}

	if (! pEqualitySet-> GaussianElimination ())
		return NULL;

	/*
	 * Set up the relationship between variables to be eliminated
	 * and the equality used to substitute out those variables.
	 */
	pElimVarToEquality = new WORD [m_pVariables-> Count ()];
	for (nVar = 0; nVar < m_pVariables-> Count (); nVar++)
		pElimVarToEquality [nVar] = VAR_NOT_ELIMINATED;

	nEquality = 0;
	for (nVar = 0; nVar < m_pVariables-> Count (); nVar++)
	{
		if (nEquality >= pEqualitySet-> m_Count)
			break;

		pEquality = & pEqualitySet-> m_pEquations [nEquality];
		if (Approx(pEquality-> m_pVarCoefs [nVar],
				   1.0, LEEWAY))
		{
			pElimVarToEquality [nVar] = nEquality;
			nEquality++;
		}
	}
	Rank = nEquality;

	/*****************************************************************
	 * Build a new symbol set for all variables that were not
	 * eliminated.
	 ****************************************************************/
	pRemainingVariables = new CSymbolSet_ (
			m_pVariables-> Count () - Rank);

	nRemainingVar = 0;
	for (nVar = 0; nVar < m_pVariables-> Count (); nVar++)
	{
		if (pElimVarToEquality [nVar] == VAR_NOT_ELIMINATED)
		{
			pRemainingVariables-> Assign (nRemainingVar,
										  m_pVariables-> GetName (nVar));
			nRemainingVar++;
		}
	}

	/*****************************************************************
	 * Build a new equation set by (1) eliminating all equalities from
	 * the original set, and (2) substituting out all eliminated
	 * variables (Because all variables are implicitly >= 0, add new
	 * inequalities for each removed variable corresponding to this
	 * constraint).
	 ****************************************************************/

	pInequalitySet = new CEquationSet_ (pRemainingVariables,
										m_pParameters,
										m_Count - EqualityCnt + Rank);

	/*
	 * Copy the old inequalities, substituting out the eliminated variables.
	 */
	pWorkEqn = new CEquation_ (m_pVariables, m_pParameters);
	nInequality = 0;
	for (nEqn = 0; nEqn < m_Count; nEqn++)
	{
		pEquation = & m_pEquations [nEqn];
		if (pEquation-> m_RelationToZero != RTZ_Equal)
		{
			pWorkEqn-> Copy (pEquation);
			for (nVar = 0; nVar < m_pVariables-> Count (); nVar++)
			{
				if (pElimVarToEquality [nVar] != VAR_NOT_ELIMINATED)
				{
					nEquality = pElimVarToEquality [nVar];
					pEquality = & pEqualitySet-> m_pEquations [nEquality];

					pWorkEqn-> FactorAdd (pEquality, 
										  - pWorkEqn-> m_pVarCoefs [nVar]);
				}
			}

			/*
			 * Copy the reduced equation into the inequality set.
			 */
			pInequality = & pInequalitySet-> m_pEquations [nInequality];

			if (pInequality-> Projection (pWorkEqn) == ProjectDataLoss)
				Rprintf ("ERROR: non-zero coefficient for an eliminated variable.\n");

			nInequality++;
		}
	}

	/*
	 * Create inequalities corresponding to the implicit constraint that all
	 * eliminated variables must have been greater than or equal to zero.
	 */
	for (nVar = 0; nVar < m_pVariables-> Count (); nVar++)
	{
		nEquality = pElimVarToEquality [nVar];
		if (nEquality != VAR_NOT_ELIMINATED)
		{
			pEquality = & pEqualitySet-> m_pEquations [nEquality];
			pInequality = & pInequalitySet-> m_pEquations [nInequality];

			/*
			 * Projection will drop the coefficient of the eliminated variable.
			 * Since that variable was >= 0, the rest of the equality must be
			 * <= 0;
			 */
			pInequality-> Projection (pEquality);
			pInequality-> m_RelationToZero = RTZ_EqualOrLess;

			nInequality++;
		}
	}

	if (pWorkEqn)
	{
		delete pWorkEqn;
		pWorkEqn = NULL;
	}
	if (pEqualitySet)
	{
		delete pEqualitySet;
		pEqualitySet = NULL;
	}
	if (pElimVarToEquality)
	{
		delete [] pElimVarToEquality;
		pElimVarToEquality = NULL;
	}

	/*
	 * The original array of equations for this set will be replaced
	 * by the generated set of inequalities.
	 */
	if (m_pEquations)
	{
		delete m_pEquations;
		m_pEquations = NULL;
	}
	m_pEquations = pInequalitySet-> m_pEquations;
	pInequalitySet-> m_pEquations = NULL;
	if (pInequalitySet)
	{
		delete pInequalitySet;
		pInequalitySet = NULL;
	}

	return pRemainingVariables;
}  /* CEquationSet_ :: EliminateVariables () */


BOOL CEquationSet_ :: GaussianElimination ()
{
	int		nRow;
	int		nEqn;
	int		nVar;
	int		MaxRow;
	double	MaxAbs;
	double	FTemp;

	/****************************************************
	 * All equations must be equalities.
	 ***************************************************/
	for (nEqn = 0; nEqn < m_Count; nEqn++)
	 	if (m_pEquations [nEqn]. m_RelationToZero != RTZ_Equal)
			return FALSE;

 	/****************************************************
	 * Diagonalize over all columns representing the 
	 * variable symbol coefficients.
	 ***************************************************/
	nVar = 0;
	for (nEqn = 0; nEqn < m_Count; nEqn++)
	{
		if (Approx (m_pEquations [nEqn]. m_pVarCoefs [nVar], 
					0.0, LEEWAY))
		{
			/*******************************************
			 * Make sure the pivot element is not zero.
			 ******************************************/
			MaxRow = -1;

			while (MaxRow < 0)
			{
				MaxAbs = 0.0;
				for (nRow = nEqn; nRow < m_Count; nRow++)
				{
					FTemp = m_pEquations [nRow]. m_pVarCoefs [nVar];
					FTemp = fabs (FTemp);
					if (FTemp > MaxAbs)
					{
						MaxRow = nRow;
						MaxAbs = FTemp;
					}
				}
				if (MaxRow < 0)
				{
					nVar++;
					if (nVar >= m_pVariables-> Count ())
						return TRUE;
					continue;
				}
			}
			m_pEquations [nEqn]. FactorAdd (& m_pEquations [MaxRow], 1.0);
		}
		m_pEquations [nEqn]. Divide (m_pEquations [nEqn]. m_pVarCoefs [nVar]);

		for (nRow = 0; nRow < m_Count; nRow++)
		{
			if (nRow != nEqn)
				m_pEquations [nRow]. FactorAdd (
						& m_pEquations [nEqn], 
						- m_pEquations [nRow]. m_pVarCoefs [nVar]);
		}
		nVar++;
	}

	return TRUE;
}  /* CEquationSet_ :: GaussianElimination () */

#endif // JUNK



