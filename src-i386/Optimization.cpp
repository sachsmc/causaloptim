//#include <afx.h>
#include <R.h>
//#include <windows.h>
#include <stdio.h>
#include <string>
#include <math.h>
#include <Rcpp.h>
#include "LinkedList.h"
#include "SymbolSet.h"
#include "Equation.h"
#include "EquationSet.h"
#include "Optimization.h"
#include "CustomFormat.h"
#include "Tableau.h"



#define		MAX_FILE_LINE		256

#define		MAX_PARSE_LINE		2048

#define	VAR_NOT_ELIMINATED		((WORD) 0xFFFF)
#define	VAR_ELIMINATED			((WORD) 0xFFFE)

typedef enum
{
	GLS_Success,
	GLS_Overflow,
	GLS_EndOfFile
} GetLineStatus_;


COptimization_ :: COptimization_ ()
{
	m_pOrigVariables = NULL;
	m_pOrigParameters = NULL;
	m_pOrigConstraints = NULL;
	m_pOrigObjective = NULL;
	m_Style = Opt_Minimize;
	m_pOrigInequalities = NULL;
	m_pOrigEqualities = NULL;
	m_pNonRedundantEqns = NULL;
	m_pReducedVariables = NULL;
	m_pReducedInequalities = NULL;
	m_pReducedObjective = NULL;
	m_pElimVarToEquality = NULL;
}  /* COptimization_ :: COptimization_ () */


COptimization_::~COptimization_ ()
{
	if (m_pVertices)
	{
		for (int nVertex = 0; nVertex < m_VertexCount; nVertex++)
			delete m_pVertices [nVertex];
		delete [] m_pVertices;
		m_pVertices = NULL;
	}

	if (m_pElimVarToEquality)
	{
		delete [] m_pElimVarToEquality;
		m_pElimVarToEquality = NULL;
	}

	if (m_pObjectiveExcessTerm)
	{
		delete m_pObjectiveExcessTerm;
		m_pObjectiveExcessTerm = NULL;
	}

	if (m_pReducedObjective)
	{
		delete m_pReducedObjective;
		m_pReducedObjective = NULL;
	}

	if (m_pBValues)
	{
		delete m_pBValues;
		m_pBValues = NULL;
	}

	if (m_pNoVariables)
	{
		delete m_pNoVariables;
		m_pNoVariables = NULL;
	}

	if (m_pReducedInequalities)
	{
		delete m_pReducedInequalities;
		m_pReducedInequalities = NULL;
	}

	if (m_pReducedVariables)
	{
		delete m_pReducedVariables;
		m_pReducedVariables = NULL;
	}

	if (m_pNonRedundantEqns)
	{
		delete m_pNonRedundantEqns;
		m_pNonRedundantEqns = NULL;
	}

 	if (m_pOrigEqualities)
	{
		delete m_pOrigEqualities;
		m_pOrigEqualities = NULL;
	}

	if (m_pOrigInequalities)
	{
		delete m_pOrigInequalities;
		m_pOrigInequalities = NULL;
	}

	if (m_pOrigObjective)
	{
		delete m_pOrigObjective;
		m_pOrigObjective = NULL;
	}

	if (m_pOrigConstraints)
	{
		delete m_pOrigConstraints;
		m_pOrigConstraints = NULL;
	}

	if (m_pOrigParameters)
	{
		delete m_pOrigParameters;
		m_pOrigParameters = NULL;
	}

	if (m_pOrigVariables)
	{
		delete m_pOrigVariables;
		m_pOrigVariables = NULL;
	}
}  /* COptimization_::~COptimization_ () */


GetLineStatus_ GetLine 
	(
		FILE *		p_pFile, 
		char *		p_pDestString,
		WORD		p_StringLen
	)
{
	char		szFileLine [MAX_FILE_LINE];

	char *		pEndChar = NULL;
					// Character pointer for scanning the end of a file line.

	BOOL		bLineEnd = FALSE;
					// Indicates when the line no longer continues to the next
					// line in the file.
	
	p_pDestString [0] = NULL;

	while (! bLineEnd)
	{
		/*
		 * Handle end of file condition.
		 */
		if (fgets (szFileLine, MAX_FILE_LINE, p_pFile) == NULL)
		{
			if (p_pDestString [0] == NULL)
				return GLS_EndOfFile;
			else
				return GLS_Success;
		}

		/*
		 * If the last non-space character is a backslash, the line continues to
		 * the next file line.
		 */
		pEndChar = szFileLine + strlen (szFileLine) - 1;
		while (isspace (*pEndChar) &&
			   pEndChar > szFileLine)
			pEndChar--;

		if (*pEndChar == '\\')
		{
			*pEndChar = NULL;
		}
		else
		{
			*(pEndChar+1) = NULL;
			bLineEnd = TRUE;
		}

		if (strlen (p_pDestString) + strlen (szFileLine) > p_StringLen)
			return GLS_Overflow;

		strcat (p_pDestString, szFileLine);
	}

	return GLS_Success;
	
}  /* GetLine () */


typedef enum
{
	PS_None,
	PS_Variables,
	PS_Parameters,
	PS_Constraints,
	PS_Objective
} ParseState_;

BOOL COptimization_ :: ParseFile (FILE * p_pFile)
{
	char		szParseLine [MAX_PARSE_LINE + 1];

	int			ScanCnt;
					// Number of fields converted by a scanf().

	ParseState_	ParseState = PS_None;
					// Indicates which section of the file is being parsed.

	ParseState_ OldParseState = PS_None;
					// Indicates the previous section of the file that was
					// just finished parsing.

	char *			szString;
					// String reference for adding to string lists.

	CLinkedList_	VarStrList;
					// List of variable name strings to be parsed.

	CLinkedList_	ParamStrList;
					// List of parameter name strings to be parsed.

	CLinkedList_	ConstStrList;
					// List of constraint strings to be parsed.

	char *			szObjective = NULL;
					// Objective function string.

	Symbol_			Symbol;
					// Symbol name used for parsing variables and parameters.

	char			szFirstWord [80];
					// String for accessing a character sequence from the 
					// parse string.

	BOOL			bStatus = TRUE;
					// Indicates whether the parsing was successful.
					// TRUE = successful, FALSE = unsuccessful.

	WORD			nSymbol;
					// Index into a symbol set.

	WORD			nConst;
					// Index into the set of constraints.
																					



	while (GetLine (p_pFile, szParseLine, MAX_PARSE_LINE) == GLS_Success)
	{
		/*
		 * Get the first word in the line.
		 */
		ScanCnt = sscanf (szParseLine, "%s", szFirstWord);

		/*
		 * If the line is blank or begins with a comment character (no
		 * comments may come after non-space characters), then go to 
		 * the next line.
		 */
		if (ScanCnt == EOF ||
			ScanCnt == 0)
			continue;

		if (*szFirstWord == '%')
			continue;

		/*
		 * Look for any keywords
		 */
		if (strcmp (szFirstWord, "END") == 0)
		{
			ParseState = PS_None;
			break;
		}

		if (strcmp (szFirstWord, "MAXIMIZE") == 0)
		{
			m_Style = Opt_Maximize;
			ParseState = PS_None;
			continue;
		}

		else if (strcmp (szFirstWord, "MINIMIZE") == 0)
		{
			m_Style = Opt_Minimize;
			ParseState = PS_None;
			continue;
		}

		else if (strcmp (szFirstWord, "VARIABLES") == 0)
		{
			ParseState = PS_Variables;
			continue;
		}

		else if (strcmp (szFirstWord, "PARAMETERS") == 0)
		{
			ParseState = PS_Parameters;
			continue;
		}

		else if (strcmp (szFirstWord, "CONSTRAINTS") == 0)
		{
			ParseState = PS_Constraints;
			continue;
		}

		else if (strcmp (szFirstWord, "OBJECTIVE") == 0)
		{
			ParseState = PS_Objective;
			continue;
		}

		/*
		 * Add the string to the active string list.
		 */
		switch (ParseState)
		{
		  case PS_Variables:
		  	szString = new char [strlen (szParseLine)+1];
			strcpy (szString, szParseLine);
			VarStrList. Append (szString);
			break;

		  case PS_Parameters:
		  	szString = new char [strlen (szParseLine)+1];
			strcpy (szString, szParseLine);
			ParamStrList. Append (szString);
			break;

		  case PS_Constraints:
		  	szString = new char [strlen (szParseLine)+1];
			strcpy (szString, szParseLine);
			ConstStrList. Append (szString);
			break;

		  case PS_Objective:
		  	if (szObjective)
			{
				delete [] szObjective;
				szObjective = NULL;
			}

			szObjective = new char [strlen (szParseLine)+1];
			strcpy (szObjective, szParseLine);
			break;

		  default:
		  	break;
		}
	}


	/*
	 * Parse all the lines obtained from the different sections,
	 * allocating the necessary space.
	 */
	m_pOrigVariables = new CSymbolSet_ ((WORD) VarStrList. Length ());
	nSymbol = 0;
	while ((szString = (char *) VarStrList. Dequeue ()))
	{
		if (! ParseSymbol (szString, Symbol))
		{
			Rprintf ("ERROR: Unable to successfully parse variable name.\n");
			Rprintf ("\t%s\n", szString);
			bStatus = FALSE;
			goto ClearParseData;
		}

		m_pOrigVariables-> Assign (nSymbol, Symbol);  
		nSymbol++;
		delete [] szString;
	}

 	m_pOrigParameters = new CSymbolSet_ ((WORD) ParamStrList. Length ());
	nSymbol = 0;
	while ((szString = (char *) ParamStrList. Dequeue ()))
	{
		if (! ParseSymbol (szString, Symbol))
		{
			Rprintf ("ERROR: Unable to successfully parse parameter name.\n");
			Rprintf ("\t%s\n", szString);
			bStatus = FALSE;
			goto ClearParseData;
		}

		m_pOrigParameters-> Assign (nSymbol, Symbol);  
		nSymbol++;
		delete [] szString;
	}

	m_pOrigConstraints = new CEquationSet_ (m_pOrigVariables, m_pOrigParameters,
										(WORD) ConstStrList. Length ());
	nConst = 0;
	while ((szString = (char *) ConstStrList. Dequeue ()))
	{
		if (! m_pOrigConstraints-> m_pEquations [nConst]. Parse (szString))
		{
			Rprintf ("ERROR: Unable to successfully parse constraint.\n");
			Rprintf ("\t%s\n", szString);
			bStatus = FALSE;
			goto ClearParseData;
		}

		nConst++;
		delete [] szString;
	}

	if (szObjective)
	{
		m_pOrigObjective = new CEquation_ (m_pOrigVariables, m_pOrigParameters);
		if (! m_pOrigObjective-> Parse (szObjective))
		{
			Rprintf ("ERROR: Unable to successfully parse objective function.\n");
			Rprintf ("\t%s\n", szObjective);
			bStatus = FALSE;
			goto ClearParseData;
		}
	}
	else
	{
		Rprintf ("ERROR: No Objective function was supplied.\n");
		bStatus = FALSE;
		goto ClearParseData;
	}




ClearParseData:

 	while ((szString = (char *) VarStrList. Dequeue ()))
		delete [] szString;

	while ((szString = (char *) ParamStrList. Dequeue ()))
		delete [] szString;

	while ((szString = (char *) ConstStrList. Dequeue ()))
		delete [] szString;

	if (szObjective)
		delete szObjective;
	
	return bStatus;
}  /* COptimization_ :: ParseFile () */

	
BOOL COptimization_ :: ParseFileWrap (const char* filename) {
	    bool status;
    
	    FILE * pFile; 
	    pFile = fopen(filename, "r");
	    status = ParseFile(pFile);
	    fclose(pFile);
	    return status;
	    
	}
	
	
void COptimization_ :: Optimize ()
{
}  /* COptimization_ :: Optimize () */


CEquation_ * COptimization_ :: GetSolution (WORD & p_SolutionID)
{
	return NULL;
}  /* COptimization_ :: GetSolution () */


std::string COptimization_ :: Display ()
{
	char			szString [2048];
					// Memory in which to format the display strings.

	WORD			nConst;
					// Index into the array of constraints.
    char buffer[2048];
    std::string result;
    
	/*
	 * Display all the optimization constraints.
	 */
	if (m_pOrigConstraints)
	{
		result.append ("Constraints:\n");
		for (nConst = 0; nConst < m_pOrigConstraints-> m_Count; nConst++)
		{
			m_pOrigConstraints-> m_pEquations [nConst]. BuildOutput (szString, 3, TRUE);
			sprintf (buffer, "\t%s\n", szString);
			result.append(buffer);
		}
	}
	 
	/*
	 * Display all the equality constraints.
	 */
	if (m_pOrigEqualities)
	{
		result.append ("Equalities:\n");
		for (nConst = 0; nConst < m_pOrigEqualities-> m_Count; nConst++)
		{
			m_pOrigEqualities-> m_pEquations [nConst]. BuildOutput (szString, 3, TRUE);
			sprintf (buffer, "\t%s\n", szString);
			result.append(buffer);
		}
	}
	 
	/*
	 * Display all the inequality constraints.
	 */
	if (m_pOrigInequalities)
	{
		result.append ("Inequalities:\n");
		for (nConst = 0; nConst < m_pOrigInequalities-> m_Count; nConst++)
		{
			m_pOrigInequalities-> m_pEquations [nConst]. BuildOutput (szString, 3, TRUE);
			sprintf (buffer, "\t%s\n", szString);
			result.append(buffer);
		}
	}
	 
	/*
	 * Display the objective function to be optimized.
	 */
	if (m_pOrigObjective)
	{
		result.append ("Objective:\n");
		m_pOrigObjective-> BuildOutput (szString, 3, TRUE);
		sprintf (buffer, "\t%s\n", szString);
		result.append(buffer);
	}

	/*
	 * Display all the reduced inequality constraints.
	 */
	if (m_pReducedInequalities)
	{
		result.append ("Reduced Inequalities:\n");
		for (nConst = 0; nConst < m_pReducedInequalities-> m_Count; nConst++)
		{
			m_pReducedInequalities-> m_pEquations [nConst]. BuildOutput (szString, 3, TRUE);
			sprintf (buffer, "\t%s\n", szString);
			result.append(buffer);
		}
	}
	 
	/*
	 * Display the reduced objective function to be optimized.
	 */
	if (m_pReducedObjective)
	{
		result.append ("Reduced Objective:\n");
		m_pReducedObjective-> BuildOutput (szString, 3, TRUE);
		sprintf (buffer, "\t%s\n", szString);
		result.append(buffer);
	}

	/*
	 * Display the objective function's excess terms.
	 */
	if (m_pObjectiveExcessTerm)
	{
		result.append ("Reduced Objective's Excess Terms:\n");
		m_pObjectiveExcessTerm-> BuildOutput (szString, 3, TRUE);
		sprintf (buffer, "\t%s\n", szString);
		result.append(buffer);
	}

	/*
	 * Display the right hand side terms of the reduced inequalities.
	 */
	if (m_pBValues)
	{
		result.append ("Right-hand side of Reduced Inequalities:\n");
		for (nConst = 0; nConst < m_pBValues-> m_Count; nConst++)
		{
			m_pBValues-> m_pEquations [nConst]. BuildOutput (szString, 3, TRUE);
			sprintf (buffer, "\t%s\n", szString);
			result.append(buffer);
		}
	}
	 
	 return result;

}  /* COptimization_ :: Display () */


void COptimization_ :: CategorizeConstraints ()
{
	int			EqualityCount = 0;
				// Number of equalities in the set of constraints.

	int			InequalityCount = 0;
				// Number of inequalities in the set of constraints.

	int			nEquality;
				// Index into the set of equality constraints.

	int			nInequality;
				// Index into the set of inequality constraints.

	int			nConstraint;
				// Index into the original set of constraints.

	for (nConstraint = 0; nConstraint < m_pOrigConstraints-> m_Count; nConstraint++)
	{
		switch (m_pOrigConstraints-> m_pEquations [nConstraint]. m_RelationToZero)
		{
			case RTZ_Equal:
				EqualityCount++;
				break;

			case RTZ_EqualOrGreater:
			case RTZ_EqualOrLess:
				InequalityCount++;
				break;

			default:
				break;
		}
	}

	/*
	 * Allocate space for the partitioned constraints.
	 */
	m_pOrigEqualities = new CEquationSet_ (m_pOrigVariables, 
			m_pOrigParameters, EqualityCount);
	m_pOrigInequalities = new CEquationSet_ (m_pOrigVariables, 
			m_pOrigParameters, InequalityCount);

	/*
	 * Copy the constraints into their respective equation category.
	 */
	nEquality = 0;
	nInequality = 0;
	for (nConstraint = 0; nConstraint < m_pOrigConstraints-> m_Count; nConstraint++)
	{
		switch (m_pOrigConstraints-> m_pEquations [nConstraint]. m_RelationToZero)
		{
			case RTZ_Equal:
				m_pOrigEqualities-> m_pEquations [nEquality]. Copy (
						& m_pOrigConstraints-> m_pEquations [nConstraint]);
				nEquality++;
				break;

			case RTZ_EqualOrGreater:
			case RTZ_EqualOrLess:
				m_pOrigInequalities-> m_pEquations [nInequality]. Copy (
						& m_pOrigConstraints-> m_pEquations [nConstraint]);
				nInequality++;
				break;

			default:
				break;
		}
	}


}  /* COptimization_ :: CategorizeConstraints () */


void COptimization_ :: GaussianElimination ()
{
	int		nRow;
	int		nEqn;
	int		nVar;
	int		MaxRow;
	double	MaxAbs;
	double	FTemp;
	CEquation_ *	pEquations = m_pOrigEqualities-> m_pEquations;
	CEquation_ *	pEquality;
	int				EqnCnt = m_pOrigEqualities-> m_Count;
	int				VarCnt = m_pOrigEqualities-> m_pVariables-> Count ();
	int				Rank;
	int				nEquality;
	int				nInequality;
	int				nRemainingVar;
	CEquation_ *	pWorkEqn;
	CEquation_ *	pEquation;
	CEquation_ *	pInequality;
	BOOL			bDiagonalized = FALSE; 

	/****************************************************
	 * All equations must be equalities.
	 ***************************************************/
	for (nEqn = 0; nEqn < EqnCnt; nEqn++)
	 	if (pEquations [nEqn]. m_RelationToZero != RTZ_Equal)
			return;

 	/****************************************************
	 * Diagonalize over all columns representing the 
	 * variable symbol coefficients.
	 ***************************************************/
	nVar = 0;
	for (nEqn = 0; nEqn < EqnCnt; nEqn++)
	{
		if (Approx (pEquations [nEqn]. m_pVarCoefs [nVar], 
					0.0, LEEWAY))
		{
			/*******************************************
			 * Make sure the pivot element is not zero.
			 ******************************************/
			MaxRow = -1;

			while (MaxRow < 0)
			{
				MaxAbs = 0.0;
				for (nRow = nEqn; nRow < EqnCnt; nRow++)
				{
					FTemp = pEquations [nRow]. m_pVarCoefs [nVar];
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
					if (nVar >= VarCnt)
					{
						goto LBL_Diagonalized;
					}
					continue;
				}
			}
			pEquations [nEqn]. FactorAdd (& pEquations [MaxRow], 1.0);
		}
		pEquations [nEqn]. Divide (pEquations [nEqn]. m_pVarCoefs [nVar]);

		for (nRow = 0; nRow < EqnCnt; nRow++)
		{
			if (nRow != nEqn)
				pEquations [nRow]. FactorAdd (
						& pEquations [nEqn], 
						- pEquations [nRow]. m_pVarCoefs [nVar]);
		}
		nVar++;
	}
LBL_Diagonalized:

	/*
	 * Set up the relationship between variables to be eliminated
	 * and the equality used to substitute out those variables.
	 */
	m_pElimVarToEquality = new WORD [VarCnt];
	for (nVar = 0; nVar < VarCnt; nVar++)
		m_pElimVarToEquality [nVar] = VAR_NOT_ELIMINATED;

	nEquality = 0;
	for (nVar = 0; nVar < VarCnt; nVar++)
	{
		if (nEquality >= EqnCnt)
			break;

		pEquality = & pEquations [nEquality];
		if (Approx(pEquality-> m_pVarCoefs [nVar],
				   1.0, LEEWAY))
		{
			m_pElimVarToEquality [nVar] = nEquality;
			nEquality++;
		}
	}
	Rank = nEquality;

	/*****************************************************************
	 * Build a new symbol set for all variables that were not
	 * eliminated.
	 ****************************************************************/
	m_pReducedVariables = new CSymbolSet_ (VarCnt - Rank);

	nRemainingVar = 0;
	for (nVar = 0; nVar < VarCnt; nVar++)
	{
		if (m_pElimVarToEquality [nVar] == VAR_NOT_ELIMINATED)
		{
			m_pReducedVariables-> Assign (nRemainingVar,
					m_pOrigEqualities-> m_pVariables-> GetName (nVar));
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

	m_pReducedInequalities = new CEquationSet_ (m_pReducedVariables,
			m_pOrigParameters, m_pOrigInequalities-> m_Count + Rank);

	/*
	 * Copy the old inequalities, substituting out the eliminated variables.
	 */
	pWorkEqn = new CEquation_ (m_pOrigVariables, m_pOrigParameters);
	nInequality = 0;
	for (nEqn = 0; nEqn < m_pOrigInequalities-> m_Count; nEqn++)
	{
		pEquation = & m_pOrigInequalities-> m_pEquations [nEqn];

		pWorkEqn-> Copy (pEquation);
		for (nVar = 0; nVar < VarCnt; nVar++)
		{
			if (m_pElimVarToEquality [nVar] != VAR_NOT_ELIMINATED)
			{
				nEquality = m_pElimVarToEquality [nVar];
				pEquality = & pEquations [nEquality];

				pWorkEqn-> FactorAdd (pEquality, 
									  - pWorkEqn-> m_pVarCoefs [nVar]);
			}
		}

		/*
		 * Copy the reduced equation into the inequality set.
		 */
		pInequality = & m_pReducedInequalities-> m_pEquations [nInequality];

		if (pInequality-> Projection (pWorkEqn) == ProjectDataLoss)
			Rprintf ("ERROR: non-zero coefficient for an eliminated variable.\n");

		nInequality++;
	}

	/*
	 * Create inequalities corresponding to the implicit constraint that all
	 * eliminated variables must have been greater than or equal to zero.
	 */
	for (nVar = 0; nVar < VarCnt; nVar++)
	{
		nEquality = m_pElimVarToEquality [nVar];
		if (nEquality != VAR_NOT_ELIMINATED)
		{
			pEquality = & m_pOrigEqualities-> m_pEquations [nEquality];
			pInequality = & m_pReducedInequalities-> m_pEquations [nInequality];

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

	m_pReducedObjective = new CEquation_ (m_pReducedVariables,
			m_pOrigParameters);

	/*
	 * Generate the reduced form of the objective function.
	 */
	pWorkEqn-> Copy (m_pOrigObjective);
	for (nVar = 0; nVar < VarCnt; nVar++)
	{
		if (m_pElimVarToEquality [nVar] != VAR_NOT_ELIMINATED)
		{
			nEquality = m_pElimVarToEquality [nVar];
			pEquality = & pEquations [nEquality];

			pWorkEqn-> FactorAdd (pEquality, 
					- pWorkEqn-> m_pVarCoefs [nVar]);
		}
	}
	if (m_pReducedObjective-> Projection (pWorkEqn) == ProjectDataLoss)
		Rprintf ("ERROR: non-zero coefficient for an eliminated variable.\n");

	delete pWorkEqn;

}  /* COptimization_ :: GaussianElimination () */


std::string COptimization_ :: EnumerateVertices ()
{
	int			Rows;
	int			Columns;
	int			VarCnt;

	CTableau *	pTableau;


	int			nInequality;
	int			nVar;
	int			nRow;
	int			nParam;

	int			nVertex;
				// Index into the enumerated vertex array.

	Pdouble_ *	pMatrixA;
	double *	pVectorB;

	Label_ *	pLabels;

	BOOL		bResult;
				// General boolean return value for checking the
				// return status of functions.
    std::string result;
    char buffer[1024];


	VarCnt = m_pReducedInequalities-> m_pVariables-> Count ();
	Columns = m_pReducedInequalities-> m_Count;
	Rows = VarCnt + Columns + 1;

	/*
	 * If we are trying to maximize, then use the additive inverse of the
	 * objective function.
	 */
	if (m_Style == Opt_Maximize)
		m_pReducedObjective-> Negate ();

	/*
	 * Make sure that all the inequality relations are greater than or equal to.
	 */
	for (nInequality = 0; nInequality < Columns; nInequality++)
	{
		m_pReducedInequalities-> m_pEquations [nInequality]. 
				ForceForm (RTZ_EqualOrGreater); 
	}

	/*
	 * Create the value set corresponding to the right hand side of the greater than
	 * or equal to relations in the reduced inequalities.
	 */
	m_pNoVariables = new CSymbolSet_ (0);
	m_pBValues = new CEquationSet_ (m_pNoVariables,
			m_pOrigParameters, m_pReducedInequalities-> m_Count);
	for (nInequality = 0; nInequality < m_pReducedInequalities-> m_Count; nInequality++)
	{
		if (m_pReducedInequalities-> m_pEquations [nInequality]. m_RelationToZero ==
				RTZ_EqualOrGreater)
		{
			for (nParam = 0; nParam < m_pOrigParameters-> Count (); nParam++)
			{
				m_pBValues-> m_pEquations [nInequality]. m_pParamCoefs [nParam] =
						- m_pReducedInequalities-> m_pEquations [nInequality]. m_pParamCoefs [nParam];
			}
			m_pBValues-> m_pEquations [nInequality]. m_ConstantTerm =
					- m_pReducedInequalities-> m_pEquations [nInequality]. m_ConstantTerm;
		}

		if (m_pReducedInequalities-> m_pEquations [nInequality]. m_RelationToZero ==
				RTZ_EqualOrLess)
		{
			for (nParam = 0; nParam < m_pOrigParameters-> Count (); nParam++)
			{
				m_pBValues-> m_pEquations [nInequality]. m_pParamCoefs [nParam] =
						m_pReducedInequalities-> m_pEquations [nInequality]. m_pParamCoefs [nParam];
			}
			m_pBValues-> m_pEquations [nInequality]. m_ConstantTerm =
					m_pReducedInequalities-> m_pEquations [nInequality]. m_ConstantTerm;
		}

		m_pBValues-> m_pEquations [nInequality]. m_RelationToZero = RTZ_Value;
	}

	/*
	 * Create the value corresponding to the nonvariable terms in the 
	 * objective function.
	 */
	m_pObjectiveExcessTerm = new CEquation_ (m_pNoVariables,
			m_pOrigParameters);
	for (nParam = 0; nParam < m_pOrigParameters-> Count (); nParam++)
	{
		m_pObjectiveExcessTerm-> m_pParamCoefs [nParam] =
				m_pReducedObjective-> m_pParamCoefs [nParam];
	}
	m_pObjectiveExcessTerm-> m_ConstantTerm =
			m_pReducedObjective-> m_ConstantTerm;

	/*
	 * Create labels for the dual linear programming problem.
	 */
	pLabels = new Label_ [Columns];
	for (nInequality = 0; nInequality < Columns; nInequality++)
	{
		sprintf (pLabels [nInequality], "y%03d", nInequality + 1);
	}


	/*
	 * Set up the A matrix for the vertex enumeration (see Mattheiss paper).
	 */
	pMatrixA = new Pdouble_ [Rows];
	for (nRow = 0; nRow < Rows; nRow++)
	{
		pMatrixA [nRow] = new double [Columns];
		memset (pMatrixA [nRow], 0, Columns * sizeof (double));

	}

	for (nInequality = 0; nInequality < Columns; nInequality++)
	{
		for (nVar = 0; nVar < VarCnt; nVar++)
		{
			pMatrixA [nVar][nInequality] = 
					m_pReducedInequalities-> m_pEquations [nInequality]. 
					m_pVarCoefs [nVar];
		}
		pMatrixA [VarCnt + nInequality][nInequality] = -1.0;
		pMatrixA [Rows - 1][nInequality] = 1.0;
	}
		    
	result.append ("Matrix A\n");
	for (nVar = 0; nVar < Rows; nVar++)
	{
		result.append ("\t");
		for (nInequality = 0; nInequality < Columns; nInequality++)
		{
			sprintf (buffer, "%5.2f ", pMatrixA [nVar][nInequality]);
		    result.append(buffer);
		}
		result.append("\n");
	}
	result.append ("\n");

	/*
	 * Set up the B matrix vector (see Mattheiss paper).
	 */
	pVectorB = new double [Rows];
	memset (pVectorB, 0, Rows * sizeof (double));
	for (nVar = 0; nVar < VarCnt; nVar++)
		pVectorB [nVar] = m_pReducedObjective-> m_pVarCoefs [nVar];
	pVectorB [Rows - 1] = Columns * 50;

	result.append ("Vector B\n");
	for (nVar = 0; nVar < Rows; nVar++)
	{
		sprintf (buffer, "\t%5.2f\n", pVectorB [nVar]);
	    result.append(buffer);
	}
	result.append ("\n");


	pTableau = new CTableau (Columns, Rows,
							pLabels, pMatrixA, pVectorB);
 
	result.append(pTableau -> VertexEnumerate ());

	m_VertexCount = pTableau-> VertexCount ();
	m_pVertices = new Pdouble_ [m_VertexCount];
	for (nVertex = 0; nVertex < m_VertexCount; nVertex++)
	{
		m_pVertices [nVertex] = new double [Columns];
		bResult = pTableau-> GetVertex (nVertex, m_pVertices [nVertex], Columns);
	}

	delete pTableau;
	delete [] pLabels;
	delete [] pVectorB;
	for (nRow = 0; nRow < Rows; nRow++)
		delete [] pMatrixA [nRow];
	delete [] pMatrixA;
	return result;

}  /* COptimization_ :: EnumerateVertices () */


std::string COptimization_::OutputOptimum ()
{
	CEquation_ *		pValue;
						// Value to accumulate one of the solution terms.

	int					nVertex;
						// Index into the set of extreme vertices.

	int					nBValue;

	char				szSolutionTerm [1024];
	
	std::string         result;

	if (m_Style == Opt_Maximize)
		result.append ("\nMIN {\n");
	else
		result.append ("\nMAX {\n");

	pValue = new CEquation_ (m_pNoVariables, m_pOrigParameters);
	for (nVertex = 0; nVertex < m_VertexCount; nVertex++)
	{
		pValue-> Copy (m_pObjectiveExcessTerm);
		for (nBValue = 0; nBValue < m_pBValues-> m_Count; nBValue++)
		{
			pValue-> FactorAdd (& m_pBValues-> m_pEquations [nBValue],
					m_pVertices [nVertex][nBValue]);
		}
		if (m_Style == Opt_Maximize)
			pValue-> Negate ();

		pValue-> BuildOutput (szSolutionTerm, 3, TRUE);
		result.append (szSolutionTerm);
		result.append ("\n");
	}
	result.append ("}\n\n");

	delete pValue;
	return result;
}  /* COptimization_::OutputOptimum () */

