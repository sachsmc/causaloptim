#ifndef OPTIMIZATION_H
#define OPTIMIZATION_H


#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <Rcpp.h>
#include "SymbolSet.h"
#include "Equation.h"
#include "EquationSet.h"


typedef double *	Pdouble_;

typedef enum OptimizeStyle_
{
	Opt_Minimize,	// Minimize objective function.
	Opt_Maximize	// Maximize objective function.
};


class COptimization_
{

public:

	CSymbolSet_ *	m_pOrigVariables;
					// Set of variable names.

	CSymbolSet_ *	m_pOrigParameters;
					// Set of parameter names.

	CEquationSet_ *	m_pOrigConstraints;
					// Array of constraint equations.

	CEquation_ *	m_pOrigObjective;
					// Objective function to be optimized under the set of
					// constraint equations.

	OptimizeStyle_	m_Style;
					// Which way to optimize (see OptimizeStyle_).

	CEquationSet_ *	m_pOrigInequalities;
					// Set of inequalities from the original constraints.

	CEquationSet_ *	m_pOrigEqualities;
					// Set of equalities from the original constraints.

	CEquationSet_ *	m_pNonRedundantEqns;
					// Set of reduced non-redundant equalities from the 
					// original constraints.

	CSymbolSet_ *	m_pReducedVariables;
					// Set of variables remaining after reducing equations by
					// non-redundant equalities.

	CEquationSet_ *	m_pReducedInequalities;
					// Inequality constraints after redundant variables have been
					// eliminated.

	CSymbolSet_ *	m_pNoVariables;
					// Empty variable set.

	CEquationSet_ *	m_pBValues;
					// Right hand side values of the reduced inequalities.

	CEquation_ *	m_pReducedObjective;
					// Objective function after redundant variables have been
					// eliminated.

	CEquation_ *	m_pObjectiveExcessTerm;
					// Nonvariable terms in the reduced objective function.

	WORD *			m_pElimVarToEquality;
					// Array that maps eliminated variables to the
					// equality used to substitute out that
					// variable.  Allocated.
					// A value of VAR_NOT_ELIMINATED if a variable
					// is not to be eliminated.


	Pdouble_ *		m_pVertices;
					// Array of all extreme vertices in the constraint space
					// of the dual linear programming problem.

	int				m_VertexCount;
					// Number of extreme vertices in m_pVertices.

	COptimization_ ();

	~COptimization_ ();

	BOOL ParseFile (FILE * p_pFile);
	
	BOOL ParseFileWrap (const char* filename);

	void CategorizeConstraints ();

	void GaussianElimination ();

	std::string EnumerateVertices ();

	void Optimize ();

	CEquation_ * GetSolution (WORD & p_SolutionID);

	std::string OutputOptimum ();

	std::string Display ();

};  /* class Optimization_ */


#endif // OPTIMIZATION_H
