//#include <afx.h>
#include <R.h>
//#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <math.h>
#include "MyUtils.h"
#include "Tableau.h"

#define		AUG_COST	-1000000.00
#define		LEEWAY		((double) 0.00001)
#define		LINE_LEN	1024
#define		MAX_SOL		50.00


float SolFactor[3] =		// Indexed by TERM_SIGN
		{-1.0f,  0.0f, +1.0f};

float SymFactor[3] =		// Indexed by TERM_SIGN
		{+1.0f, +1.0f, -1.0f};


void Approx (double & p_Value, double p_Point)
{
	if (p_Value < p_Point + LEEWAY &&
		p_Value > p_Point - LEEWAY)
		p_Value = p_Point;
}  /* Approx () */


CTableau :: CTableau (
				int 		p_ParamCnt,
				int			p_Rows,
				Label_		p_pParamNames[],
				pFloat_ *	p_pA,
				double *	p_pb
					//************************
					// Ax <= b
					//************************
				)
{
	Setup (p_ParamCnt, p_Rows, p_pParamNames, p_pA, p_pb);
}  /* CTableau :: CTableau () */


void CTableau :: Setup (
				int 		p_ParamCnt,
				int			p_Rows,
				Label_		p_pParamNames[],
				pFloat_ *	p_pA,
				double *	p_pb
					//************************
					// Ax <= b
					//************************
				)
{
	int		nParam;
	int		nVar;
	int		nRow;
	int		nBasis;
	int		nAug;

	double	Coef;
	double	YCoef;


	m_pEnumList = NULL;
	m_pEnumCrnt = NULL;
	m_EnumListLen = 0;

	memset (m_pVertices, NULL, sizeof (m_pVertices));

	// Check that the number of rows is greater than the number of parameters.
	//
	if (p_Rows <= p_ParamCnt)
	{
		//Rprintf ("ERROR: Let A be a mxn matrix.  m must be greater than n.\n");
		error ("ERROR: Let A be a mxn matrix.  m must be greater than n.\n");
	}


	//
	//****************************************************************
	// Get the names of all the parameters.
	//

	m_ParamCnt = p_ParamCnt;
	m_pParamNames = new Label_ [m_ParamCnt];
	for (nParam = 0; nParam < m_ParamCnt; nParam++)
		strcpy (m_pParamNames [nParam], p_pParamNames [nParam]);


	//************************
	// Determine how many augmented variables must be added.
	// This is just the number of negative entries in the solution
	// vector 'b'.

	m_AugCnt = 0;
	for (nRow = 0; nRow < p_Rows; nRow++)
	{
		if (p_pb [nRow] < 0.0)
			m_AugCnt++;
	}


	//**********************************************************
	// Evaluate the number of variables required and label them.
	

	m_VarCnt = 1 + m_ParamCnt + 1 + p_Rows + m_AugCnt;

	m_pVarLabels = new Label_ [m_VarCnt];
	strcpy (m_pVarLabels [0], "nz");

	for (nParam = 0; nParam < m_ParamCnt; nParam++)
	{
		nVar = 1 + nParam;
		strcpy (m_pVarLabels [nVar], p_pParamNames [nParam]);
	}

	strcpy (m_pVarLabels [1 + m_ParamCnt], "y");

	for (nRow = 0; nRow < p_Rows; nRow++)
	{
		nVar = 1 + m_ParamCnt + 1 + nRow;
		snprintf (m_pVarLabels [nVar],1024, "$%02d", nRow);
	}

	for (nAug = 0; nAug < m_AugCnt; nAug++)
	{
		nVar = 1 + m_ParamCnt + 1 + p_Rows + nAug;
		snprintf (m_pVarLabels [nVar],1024, "@%02d", nAug);
	}


	//**************************************
	// Determine the size of the basis for this problem.

	m_BasisCnt = p_Rows + 1;


	//***********************************************************
	// Done incorporating optimization problem parameters.  
	// Allocate the space for the Tableau.
	//***********************************************************

	m_pOrigBasisVars = new int [m_BasisCnt];
	memset (m_pOrigBasisVars, 0, m_BasisCnt * sizeof (int));
	m_pBasisVars = new int [m_BasisCnt];
	memset (m_pBasisVars, 0, m_BasisCnt * sizeof (int));
     
    m_pOrigSolution = new double [m_BasisCnt];
	memset (m_pOrigSolution, 0, m_BasisCnt * sizeof (double));
	m_pSolution = new double [m_BasisCnt];
	memset (m_pSolution, 0, m_BasisCnt * sizeof (double));
	
	m_pCj = new double [m_VarCnt];
	memset (m_pCj, 0, m_VarCnt * sizeof (double));

	m_pOrigTable = new pFloat_ [m_BasisCnt];
	m_pTable = new pFloat_ [m_BasisCnt];
	
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		m_pOrigTable [nBasis] = new double [m_VarCnt];
		memset (m_pOrigTable [nBasis], 0, m_VarCnt * sizeof(double));
		m_pTable [nBasis] = new double [m_VarCnt];
		memset (m_pTable [nBasis], 0, m_VarCnt * sizeof(double));
	}
	
	//******************************
	// Instantiate the Tableau from the given matrix, A.

	m_pOrigTable [0][0] = 1.0;
	m_pOrigTable [0][1 + m_ParamCnt] = 1.0;
	m_pCj [1 + m_ParamCnt] = 1.0;

	nAug = 0;
	for (nRow = 0; nRow < p_Rows; nRow++)
	{
		nBasis = nRow + 1;

		//****************************
		// Determine the set of variables to use in the basis,
		// and set the table coefficients for the augmented
		// variables.
		// Also determine the coefficient to be used for the
		// current basis row.
		//***************************
		if (p_pb [nRow] < 0)
		{
				//***********************************
				// The slack variable will have a negative
				// coefficient; use an augmented variable
				// in the basis.
				//***********************************
			Coef = -1.0;
			nVar = 1 + m_ParamCnt + 1 + p_Rows + nAug;
			m_pOrigTable [nBasis][nVar] = 1.0;
			m_pCj [nVar] = AUG_COST;
			m_pOrigBasisVars [nBasis] = nVar;
			nAug++;
		}
		else
		{
				//************************************
				// Use the slack variable in the basis.
				//************************************
			nVar = 1 + m_ParamCnt + 1 + nRow;
			m_pOrigBasisVars [nBasis] = nVar;
			Coef = 1.0;
		}

		//**************************************
		// Set the entries corresponding to the user defined
		// parameters.
		// At the same time compute the coefficient for the
		// 'y' variable.
		//**************************************
		YCoef = 0.0;
		for (nParam = 0; nParam < m_ParamCnt; nParam++)
		{
			nVar = 1 + nParam;
			m_pOrigTable [nBasis][nVar] = Coef * p_pA [nRow][nParam];
			YCoef += p_pA [nRow][nParam] *
					 p_pA [nRow][nParam];
		}
		nVar = 1 + m_ParamCnt;
		m_pOrigTable [nBasis][nVar] = Coef * sqrt (YCoef);


		//**************************************
		// Set the entries corresponding to the slack variables.
		//**************************************
		nVar = 1 + m_ParamCnt + 1 + nRow;
		m_pOrigTable [nBasis][nVar] = Coef;

		//*****************************************
		// Set the solution value to be non-negative.
		//****************************************
		m_pOrigSolution [nBasis] = Coef * p_pb [nRow];
	}

	//*******************************
	// Copy the original tableau over to the working copy.
	//*******************************
	Reset ();	

	m_VertexCnt = 0;
	m_pSlackFlag = new char [m_BasisCnt - 1];
	memset (m_pSlackFlag, 0, (m_BasisCnt - 1) * sizeof (char));
}  /* CTableau :: Setup () */ 




CTableau :: CTableau (FILE * p_pFile)
{
	char		szLine [LINE_LEN];
	char		szWord [LINE_LEN];

	char		bMinimize;
	int			ParamCnt;
	int			ConstCnt;
	Label_ *	pParamNames;
	String_ *	pszConstraints;
	String_		szObjective;

	int			nParam;
	int			nConst;

	int			ScanRslt;
	char* objGet;

	//*****************************************************************
	// Determine whether to Minimize or Maximize the objective
	// function.
	//
	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//printf ("ERROR: didn't find Min/Max specifier\n");
			error ("ERROR: didn't find Min/Max specifier\n");
		}

		sscanf (szLine, "%s", szWord);

		if (strcmp (szWord, "MAXIMIZE") == 0)
		{
			bMinimize = 0;
			break;
		}
		if (strcmp (szWord, "MINIMIZE") == 0)
		{
			bMinimize = 1;
			break;
		}
	}


	//
	//****************************************************************
	// Get the number of parameters.
	//

	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//Rprintf ("ERROR: didn't find 'PARAMETERS'\n");
			error ("ERROR: didn't find 'PARAMETERS'\n");
		}
		sscanf (szLine, "%s", szWord);
		if (strcmp (szWord, "PARAMETERS") == 0)
			break;
	}

	ParamCnt = 0;
	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//Rprintf ("ERROR: no 'CONSTRAINTS' line\n");
			error ("ERROR: no 'CONSTRAINTS' line\n");
		}
		if (sscanf (szLine, "%s", szWord) >= 0)
		{
			if (strcmp (szWord, "CONSTRAINTS") == 0)
				break;
			else
				ParamCnt++;
		}
	}

	pParamNames = new Label_ [ParamCnt];




	//
	//****************************************************************
	// Get the number of constraints.
	//

	ConstCnt = 0;
	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//Rprintf ("ERROR: no 'OBJECTIVE' line\n");
			error ("ERROR: no 'OBJECTIVE' line\n");
		}
		if (sscanf (szLine, "%s", szWord) >= 0)
		{
			if (strcmp (szWord, "OBJECTIVE") == 0)
				break;
			else
				ConstCnt++;
		}
	}

	pszConstraints = new String_ [ConstCnt];


	//
	//******************************************************
	// Copy the objective function
	//

	objGet = fgets (szObjective, LINE_LEN, p_pFile);



	// Go back to beginning of file.
	rewind (p_pFile);


	//
	//************************************************
	// Find the parameter names and copy them.
	//

	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//printf ("ERROR: didn't find 'PARAMETERS'\n");
			error ("ERROR: didn't find 'PARAMETERS'\n");
		}
		sscanf (szLine, "%s", szWord);
		if (strcmp (szWord, "PARAMETERS") == 0)
			break;
	}
	nParam = 0;
	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//Rprintf ("ERROR: no 'CONSTRAINTS' line\n");
			error ("ERROR: no 'CONSTRAINTS' line\n");
		}
		if (sscanf (szLine, "%s", szWord) >= 0)
		{
			if (strcmp (szWord, "CONSTRAINTS") == 0)
				break;
			else
			{
				strcpy (pParamNames [nParam], szWord);
				nParam++;
			}
		}
	}



	//
	//****************************************
	// Find the constraints and copy them.
	//

	nConst = 0;
	while (1)
	{
		if (fgets (szLine, LINE_LEN, p_pFile) == 0)
		{
			//Rprintf ("ERROR: no 'OBJECTIVE' line\n");
			error ("ERROR: no 'OBJECTIVE' line\n");
		}

		if (sscanf (szLine, "%s", szWord) >= 0)
		{
			if (strcmp (szWord, "OBJECTIVE") == 0)
				break;
			else
			{
				strcpy (pszConstraints [nConst], szLine);
				nConst++;
			}
		}
	}


//	Setup (p_bVertices, bMinimize, ParamCnt, pParamNames,
//		   ConstCnt, pszConstraints, szObjective);


}  /* CTableau :: CTableau () */ 


//======================================
// FUNCTION:	Reset
// PURPOSE:
// 	Sets the Working Tableau equal to the Original Tableau.
//======================================
void CTableau :: Reset ()
{
	int			nBasis;
	int			nVar;
	
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		m_pSolution [nBasis] = m_pOrigSolution [nBasis];
		m_pBasisVars [nBasis] = m_pOrigBasisVars [nBasis];
	} 
	
	for (nVar = 0; nVar < m_VarCnt; nVar++)
	{
		for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
		{
			m_pTable [nBasis] [nVar] = m_pOrigTable [nBasis] [nVar];
		}
	}
}  /* CTableau :: Reset () */

 


CTableau :: ~CTableau ()
{
	CEnumRcd *	pNextRcd;
	CEnumRcd *	pEnumRcd;

	for (int nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		delete [] m_pOrigTable [nBasis];
		delete [] m_pTable [nBasis];
	}
	delete [] m_pTable;
	delete [] m_pOrigTable;

	delete [] m_pBasisVars;
	delete [] m_pOrigBasisVars;

	delete [] m_pSolution;
	delete [] m_pOrigSolution;

	delete [] m_pCj;
	delete [] m_pSlackFlag;

//	delete m_pConstraints;
//	delete m_pObjective;

	delete m_pVarLabels;
	delete m_pParamNames;

	for (int nVertex = 0; nVertex < MAX_VERTICES; nVertex++)
		if (m_pVertices [nVertex])
			delete [] m_pVertices [nVertex];
			
	pEnumRcd = m_pEnumList;
	while (pEnumRcd)
	{
		pNextRcd = pEnumRcd-> m_pNext;
		delete [] pEnumRcd-> Contents ();
		delete pEnumRcd;
		pEnumRcd = pNextRcd;
	}
}  /* CTableau :: ~CTableau () */



/*=======================================
| FUNCTION:	TradeBasis
| PURPOSE:
|	Swaps the indicated non-basis variable for the specified
|	basis variable.
========================================*/
void CTableau :: TradeBasis (int p_Basis, int p_Var)
{
	double		Pivot;
	double		Factor;
	int			nVar;
	int			nBasis;

	Pivot = m_pTable [p_Basis] [p_Var];

	m_pBasisVars [p_Basis] = p_Var;

	m_pSolution [p_Basis] /= Pivot;
	for (nVar = 0; nVar < m_VarCnt; nVar++)
	{
		m_pTable [p_Basis] [nVar] /= Pivot;
	}

	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		if (nBasis != p_Basis)
		{
			Factor = m_pTable [nBasis] [p_Var];

			m_pSolution [nBasis] -= Factor * m_pSolution [p_Basis];
			Approx (m_pSolution [nBasis], 0.0);

			for (nVar = 0; nVar < m_VarCnt; nVar++)
			{
				m_pTable [nBasis] [nVar] -= 
						Factor * m_pTable [p_Basis] [nVar];
				Approx (m_pTable [nBasis][nVar], 0.0);
			}
		}
	}
}  /* CTableau :: TradeBasis () */


std::string CTableau :: DecisionDisplay ()
{
	int		nBasis;
	int		nVar;
	char buffer[100];
	std::string result;
	
	result.append ("Basis");
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		snprintf (buffer,100, "%7s", m_pVarLabels [m_pBasisVars [nBasis]]);
	    result.append (buffer);
	}
	result.append ("\n");

	for (nVar = 0; nVar < m_VarCnt; nVar++)
	{
			// Display only the nonbasis variables.
		for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
			if (m_pBasisVars [nBasis] == nVar)
				break;

		if (nBasis < m_BasisCnt)
			continue;

		snprintf (buffer,100, "%7s", m_pVarLabels [nVar]);
		result.append(buffer); 
		for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
		{
			snprintf (buffer, 100, "%7.3lf", m_pTable [nBasis][nVar]);
		    result.append(buffer);
		}
		result.append ("\n");
	}
	
	result.append ( "Sol");
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		snprintf (buffer,100, "%7.3lf", m_pSolution [nBasis]);
	    result.append(buffer); 
	}
	result.append ("\n\n");
	return result;
}  /* CTableau :: DecisionDisplay () */


/*=================================
| FUNCTION: DetermineSwap
| PURPOSE:
|	Determines which non-basis and basis variable to swap.
===================================*/
void CTableau :: DetermineSwap (int & p_Basis, int & p_Var)
{
	int		nBasis;
	int		nVar;
	int		nFirstAug;
	double	Contr;
	double	MaxPosContr = 0.0;
	double	Ratio;
	double	MinRatio;

	p_Var = -1;
	for (nVar = 1; nVar < m_VarCnt; nVar++)
	{
			//===============================
			// Compute zj-cj for this column
			//===============================
		Contr = m_pCj [nVar];
		for (nBasis = 1; nBasis < m_BasisCnt; nBasis++)
		{
			Contr -= m_pTable [nBasis][nVar] *
					 m_pCj [m_pBasisVars [nBasis]];
		}

		if (Contr > MaxPosContr)
		{
			MaxPosContr = Contr;
			p_Var = nVar;
		}
	}

	if (p_Var < 0)
	{
		return;
	}

	p_Basis = -1;
	MinRatio = 1000000.0;
	for (nBasis = 1; nBasis < m_BasisCnt; nBasis++)
	{
		if (m_pTable [nBasis] [p_Var] > LEEWAY)
		{
			Ratio = m_pSolution [nBasis] / m_pTable [nBasis] [p_Var];

			if (Ratio >= 0.0 &&
				Ratio < MinRatio)
			{
				MinRatio = Ratio;
				p_Basis = nBasis;
			}
		}
	}
}  /* CTableau :: DetermineSwap () */


double CTableau :: GetSolution (char * p_szVarName)
{
	for (int nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		if (strcmp (p_szVarName, m_pVarLabels [m_pBasisVars [nBasis]]) == 0)
		{
			return m_pSolution [nBasis];
		}
	}
	return 0.0;
}  /* CTableau :: GetValue () */


double CTableau :: ObjectiveValue ()
{
	return -m_pSolution [0];
} /* CTableau :: ObjectiveValue () */


char CTableau :: Optimize ()
{
	int		nVar;
	int		Var;
	int		Basis;


//#define MYDEBUG

	while (1)
	{
#ifdef MYDEBUG
		DecisionDisplay ();
#endif

		DetermineSwap (Basis, Var);

#ifdef MYDEBUG
		Rprintf ("Var: %4s,    Basis: %4s\n", 
				m_pVarLabels [Var],
				m_pVarLabels [m_pBasisVars[Basis]]);
#endif

		if (Var < 0 || Basis < 0)
			break;
		TradeBasis (Basis, Var);
	}
	return 1;
}  /* CTableau :: Optimize () */

void CTableau :: DisplayBasis ()
{
	int		nBasis;
	
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		Rprintf ("%s:  %lf\n", m_pVarLabels [m_pBasisVars [nBasis]], 
				m_pSolution [nBasis]);
	}

}  /* CTableau :: DisplayBasis () */


void CTableau :: DisplayParams ()
{
	int		nVar;
	int		nBasis;

	for (nVar = m_BasisCnt; 
		 nVar < m_BasisCnt + m_ParamCnt; nVar++)
	{
		for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
		{
			if (nVar == m_pBasisVars [nBasis])
			{
				Rprintf ("%5s:  %lf\n", m_pVarLabels [nVar],
						m_pSolution [nBasis]);
				break;
			}
		}
	}
}  /* CTableau :: DisplayParams () */


void CTableau :: DropVars ()
{

	m_VarCnt = 1 + m_ParamCnt + 1 + (m_BasisCnt - 1);
	m_AugCnt = 0;

	// Changing these values will not affect the later deallocation of the
	//	Tableau.

}  /* CTableau :: DropVars () */


//======================================
// FUNCTION:	WorkToOrig
// PURPOSE:
// 	Sets the Original Tableau equal to the Working Tableau.
//======================================
void CTableau :: WorkToOrig ()
{
	int			nBasis;
	int			nVar;
	
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		m_pOrigSolution [nBasis] = m_pSolution [nBasis];
		m_pOrigBasisVars [nBasis] = m_pBasisVars [nBasis];
	} 
	
	for (nVar = 0; nVar < m_VarCnt; nVar++)
	{
		for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
		{
			m_pOrigTable [nBasis] [nVar] = m_pTable [nBasis] [nVar];
		}
	}
}  /* CTableau :: WorkToOrig () */




WORD * CTableau :: PopFirst ()
{
	int *		pNonBasics;
	CEnumRcd *	pNextRcd;
	CEnumRcd *	pEnumRcd;
	double		CrntValue;
	double		NewValue;
	int			nNonBasis;

	if (m_pEnumList == NULL)
		return NULL;

	if (m_pEnumCrnt == NULL)
	{
		m_pEnumCrnt = m_pEnumList;
		return m_pEnumCrnt-> Contents ();
	}

	if (m_pEnumCrnt-> m_pNext == NULL)
	{
//		printf ("No more records, deleting all:\n");
//		DisplayEnumRcds ();
		return NULL;
	}

	CrntValue = m_pEnumCrnt-> Value ();
	NewValue = m_pEnumCrnt-> m_pNext-> Value ();

	m_pEnumCrnt = m_pEnumCrnt-> m_pNext;

	if (NewValue > CrntValue + LEEWAY)
	{
		//Rprintf ("ERROR: The list is not monotonically non-increasing.\n");
		error ("ERROR: The list is not monotonically non-increasing.\n");
	}

	if (NewValue < CrntValue - LEEWAY)
	{
		//=================================
		// Remove all the preceding elements in the
		// list.  They are no longer needed for eliminating
		// duplicates.
		//==================================

		pEnumRcd = m_pEnumList;
		while (pEnumRcd != m_pEnumCrnt)
		{
//			printf ("Deleting record: %6.3lf :", pEnumRcd-> Value ());
//			for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; 
//				 nNonBasis++)
//			{
//				printf (" %4s", m_pVarLabels [
//						(pEnumRcd-> Contents ()) [nNonBasis]]);
//			}
//			printf ("\n");

			pNextRcd = pEnumRcd-> m_pNext;
			delete [] pEnumRcd-> Contents ();
			delete pEnumRcd;
			pEnumRcd = pNextRcd;
			m_EnumListLen--;
		}
		m_pEnumList = m_pEnumCrnt;

//		DisplayEnumRcds ();
	}

	return m_pEnumCrnt-> Contents ();
}  /* CTableau :: PopFirst () */




void CTableau :: AddUnique (double p_Value, WORD * p_pNonBasics)
{
	CEnumRcd *	pCurrent;
	CEnumRcd **	ppPrior;
	CEnumRcd *	pNew;
	double		Value;
	int			nNonBasis;

//	printf (":v%lf", p_Value);

	ppPrior = &m_pEnumList;
	pCurrent = m_pEnumList;
	while (pCurrent != NULL)
	{
		Value = pCurrent-> Value ();
		if (p_Value < Value + LEEWAY &&
			p_Value > Value - LEEWAY)
		{
			// See if the new record is non-unique.
			// This assumes that the non-basic variables are
			// always ordered by index value.
			for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
			{
				if (p_pNonBasics [nNonBasis] != 
					(pCurrent-> Contents ())[nNonBasis])
					break;
			}
			if (nNonBasis == m_VarCnt - m_BasisCnt)
			{
				// NonBasis is not unique; do not add to list.
				delete [] p_pNonBasics;
				return;
			}
		}
		else if (p_Value > Value)
		{
			pNew = new CEnumRcd (p_Value, p_pNonBasics);
			*ppPrior = pNew;
			pNew-> m_pNext = pCurrent;
			m_EnumListLen++;
			return;
		}
		ppPrior = &(pCurrent-> m_pNext);
		pCurrent = pCurrent-> m_pNext;
	}
	pNew = new CEnumRcd (p_Value, p_pNonBasics);
	*ppPrior = pNew;
	m_EnumListLen++;
	
}  /* CTableau :: AddUnique () */


void CTableau :: DisplayEnumRcds ()
{
	CEnumRcd *	pCurrent;
	WORD *		pNonBasis;
	int			nNonBasis;

	Rprintf ("Contents of Enum List\n");
	pCurrent = m_pEnumList;
	while (pCurrent != NULL)
	{
		Rprintf ("\t%6.3lf : ", pCurrent-> Value ());

		pNonBasis = pCurrent-> Contents ();
		for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
		{
			Rprintf ("%4s ", m_pVarLabels [pNonBasis [nNonBasis]]);
		}
		if (pCurrent == m_pEnumCrnt)
			Rprintf (" **");
		Rprintf ("\n");


		pCurrent = pCurrent-> m_pNext;
	}
	
}  /* CTableau :: DisplayEnumRcds () */



std::string CTableau :: VertexEnumerate ()
{
	int			nBasis;
	int			nNonBasis;
	int			nSlack;
	WORD *		pNonBasics;
	int			nVar;
	int			PivotBasis;
	double		MinRatio;
	double		Ratio;
	double *	pNewSol;
	int *		pBasisVars;
	double		Factor;
	double		Pivot;
	char		szResponse[100];
	char        buffer[1024];
	std::string result;

	result.append ("Table before optimizing Y.\n");
	result.append ("--------------------------\n");

	pNewSol = new double [m_BasisCnt];
	pBasisVars = new int [m_BasisCnt];

	result.append(DecisionDisplay ());

	Optimize ();
	DropVars ();
	WorkToOrig ();

	AddEnumRcd (m_pBasisVars, - m_pSolution[0]);
//	DisplayEnumRcds ();

	while ((pNonBasics = PopFirst ()) != NULL)
	{
//		printf ("Analyzing: ");
//		for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
//		{
//			printf (" %4s", m_pVarLabels [pNonBasics [nNonBasis]]);
//		}
//		printf ("\n");
    R_CheckUserInterrupt();

		GenerateTableau (pNonBasics);

			// Flag all the nonbasic slack variables.  This is later used to
			//	identify relevant constraints.
		for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
		{
			m_pSlackFlag [pNonBasics [nNonBasis] - m_ParamCnt - 2] = 1;
		}

//		DecisionDisplay ();

		for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
		{
			nVar = pNonBasics [nNonBasis];

#ifdef MYDEBUG
			Rprintf ("NonBasis [%d] = %4s\n", nNonBasis, 
					m_pVarLabels [pNonBasics [nNonBasis]]);
#endif

			if (m_pTable [0][nVar] > LEEWAY)
				continue;

			//==============================
			// Find the basis to pivot on.
			//==============================
			MinRatio = 1000000.0;
			for (nBasis = 1; nBasis < m_BasisCnt; nBasis++)
			{
				if (m_pTable [nBasis] [nVar] > LEEWAY &&
					m_pSolution [nBasis] >= -LEEWAY)
				{
					Ratio = m_pSolution [nBasis] / m_pTable [nBasis] [nVar];
		
					if (Ratio >= - LEEWAY &&
						Ratio < MinRatio)
					{
						MinRatio = Ratio;
					}
				}
			}

			for (PivotBasis = 1; PivotBasis < m_BasisCnt; PivotBasis++)
			{
			    R_CheckUserInterrupt();
				if (m_pTable [PivotBasis] [nVar] <= LEEWAY)
					continue;

				Ratio = m_pSolution [PivotBasis] / 
						m_pTable [PivotBasis] [nVar];
		
				if (Ratio < -LEEWAY ||
					Ratio > MinRatio + LEEWAY)
					continue;

				if (m_pBasisVars [PivotBasis] >= 1 &&
					m_pBasisVars [PivotBasis] < 1 + m_ParamCnt)
						//==================================
						// Pivot is a user-defined parameter.
						//==================================
					continue;

				for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
				{
					pNewSol [nBasis] = m_pSolution [nBasis];
					pBasisVars [nBasis] = m_pBasisVars [nBasis];
				}


					//===============================
					// Evaluate the new solutions and basis
					// variables given the pivot point.
					//===============================
				Pivot = m_pTable [PivotBasis] [nVar];

				pBasisVars [PivotBasis] = nVar;
				pNewSol [PivotBasis] /= Pivot;

				for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
				{
					if (nBasis != PivotBasis)
					{
						Factor = m_pTable [nBasis] [nVar];
						pNewSol [nBasis] -= Factor * pNewSol [PivotBasis];
					}
				}

//				printf ("Pivot %5s for %5s\n", 
//						m_pVarLabels [nVar], 
//						m_pVarLabels [m_pBasisVars [PivotBasis]]);

				if (m_pBasisVars [PivotBasis] == 1 + m_ParamCnt)
				{
						//===================================
						// Pivot is Y.
						//===================================
	

					AddVertex (pBasisVars, pNewSol);
//					printf ("\nVERTEX:\n");
//					for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
//					{
//						printf ("\t%5s: %lf\n", 
//								m_pVarLabels [pBasisVars [nBasis]],
//								pNewSol [nBasis]);
//					}
				}

				else
				{
						//===================================
						// Pivot is a slack variable.
						//===================================

						// If Y = 0.0, then the solution corresponds to a
						//	point in the n-dim space, K; therefore, do not 
						//	add the point as a subsystem in the (n+1)-dim
						//	space (currently, I do not know if this is
						//	absolutely correct).
					if (- pNewSol [0] > LEEWAY)
					{
						AddEnumRcd (pBasisVars, - pNewSol [0]);
					}
				}
			}
		}
	}

	result.append(DisplayVertices ());

	result.append ("Irrelevant contraints:\n");
	for (nSlack = 0; nSlack < m_BasisCnt - 1; nSlack++)
	{
		if (m_pSlackFlag [nSlack] == 0)
		{
			snprintf (buffer,1024, "\t%d\n", nSlack);
		    result.append(buffer);
		}
	}

	delete [] pNewSol;
    return result;
}  /* CTableau :: VertexEnumerate () */


void CTableau :: GenerateTableau (WORD * p_pNonBasics)
{
	int		nRow;
	int		nVar;
	int		nNonBasis = 0;
	char *	pbBasis;
	int		nBasis;
	int		MaxRow;
	double	MaxAbs;
	double	FTemp;

	pbBasis = new char [m_VarCnt];

	Reset ();

	memset (pbBasis, 1, m_VarCnt * sizeof (char));
	for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
		pbBasis [p_pNonBasics [nNonBasis]] = 0;

	nBasis = 1;
	for (nVar = 1; nVar < m_VarCnt; nVar++)
	{
		if (pbBasis [nVar])
		{
			m_pBasisVars [nBasis] = nVar;
			nBasis++;
		}
	}
	if (nBasis != m_BasisCnt)
	{
		//Rprintf ("ERROR: GenerateTableau: incorrect basis count.\n");
		error ("ERROR: GenerateTableau: incorrect basis count.\n");
	}

	for (nBasis = 1; nBasis < m_BasisCnt; nBasis++)
	{
		nVar = m_pBasisVars [nBasis];
		if (m_pTable [nBasis][nVar] < LEEWAY &&
			m_pTable [nBasis][nVar] > -LEEWAY)
		{
				//===========================
				// Make sure the pivot element is not zero.
				//=============================
			MaxRow = 0;
			MaxAbs = 0.0;
			for (nRow = nBasis; nRow < m_BasisCnt; nRow++)
			{
				FTemp = m_pTable [nRow][nVar];
				FTemp = fabs(FTemp);
				if (FTemp > MaxAbs)
				{
					MaxRow = nRow;
					MaxAbs = FTemp;
				}
			}
			FactorAddRows (MaxRow, nBasis, 1.0);
		}
		DivideRow (nBasis, m_pTable [nBasis][nVar]);

		for (nRow = 0; nRow < m_BasisCnt; nRow++)
		{
			if (nRow != nBasis)
				FactorAddRows (nBasis, nRow, -m_pTable [nRow][nVar]);
		}
	}

	delete [] pbBasis;
}  /* CTableau :: GenerateTableau () */


void CTableau :: DivideRow (int p_Basis, double p_Divisor)
{
	int		nVar;

	if (p_Divisor < LEEWAY &&
		p_Divisor > -LEEWAY)
		return;

	m_pSolution [p_Basis] /= p_Divisor;

	for (nVar = 1; nVar < m_VarCnt; nVar++)
	{
		m_pTable [p_Basis][nVar] /= p_Divisor;
	}
}  /* CTableau :: DivideRow () */


void CTableau :: FactorAddRows (int p_SrcRow, int p_TgtRow, double p_Factor)
{
	int		nVar;

	m_pSolution [p_TgtRow] +=
				p_Factor * m_pSolution [p_SrcRow];

	for (nVar = 1; nVar < m_VarCnt; nVar++)
	{
		m_pTable [p_TgtRow][nVar] +=
				p_Factor * m_pTable [p_SrcRow][nVar];
	}
}  /* CTableau :: AddRows () */


void CTableau :: AddEnumRcd (int * p_pBasisVars, double p_Value)
{
	int			nVar;
	int			nBasis;
	char *		pbBasis;
	WORD *		pNonBasis;
	int			nNonBasis;

	pbBasis = new char [m_VarCnt];

	pNonBasis = new /*int*/ WORD [m_VarCnt - m_BasisCnt];

	memset (pbBasis, 0, m_VarCnt * sizeof (char));
	for (nBasis = 0; nBasis < m_BasisCnt; nBasis++)
	{
		pbBasis[p_pBasisVars [nBasis]] = 1;
	}

	nNonBasis = 0;
	for (nVar = 0; nVar < m_VarCnt; nVar++)
	{
		if (pbBasis [nVar] == 0)
		{
			pNonBasis[nNonBasis] = (WORD) nVar;
			nNonBasis++;
		}
	}

	AddUnique (p_Value, pNonBasis);

	//printf (":%d", m_EnumListLen);

	delete [] pbBasis;

//	printf ("\nAdding Enum Record: %6.3lf :", p_Value);
//	for (nNonBasis = 0; nNonBasis < m_VarCnt - m_BasisCnt; nNonBasis++)
//	{
//		printf (" %4s", m_pVarLabels [pNonBasis [nNonBasis]]);
//	}
//	printf ("\n");

//	DisplayEnumRcds ();

}  /* CTableau :: AddEnumRcd () */


void CTableau :: AddVertex (int * p_pBasisVars, double * p_pSolution)
{
	double *	pVertex;
	int			nBasis;
	int			nParam;
	int			nVertex;
	double		NewVal;
	double		OldVal;

	if (m_VertexCnt >= MAX_VERTICES)
	{
		error ("ERROR: Exceeded maximum number of vertices.\n");
		return;
	}

	pVertex = new double [m_ParamCnt];
	memset (pVertex, 0, m_ParamCnt * sizeof (double));

	for (nBasis = 1; nBasis < m_BasisCnt; nBasis++)
	{
		if (p_pBasisVars [nBasis] >= 1 &&
			p_pBasisVars [nBasis] <= m_ParamCnt)
		{
			pVertex [p_pBasisVars [nBasis] - 1] = 
					p_pSolution [nBasis];
		}
	}

	//============================
	// Check that all coordinates in the vertex are
	// not positively extreme.  Otherwise, do not add vertex.
	//============================
	for (nVertex = 0; nVertex < m_ParamCnt; nVertex++)
	{
		if (pVertex [nVertex] > MAX_SOL)
		{
			delete [] pVertex;
			//printf ("-");
			return;
		}
	}

	//============================
	// Check vertex for uniqueness.
	//============================
	for (nVertex = 0; nVertex < m_VertexCnt; nVertex++)
	{
		for (nParam = 0; nParam < m_ParamCnt; nParam++)
		{
			NewVal = pVertex [nParam];
			OldVal = m_pVertices [nVertex][nParam];
			if (NewVal < OldVal - LEEWAY ||
				NewVal > OldVal + LEEWAY)
				break;
		}

		if (nParam == m_ParamCnt)
			break;		// Duplicate found.
	}

	if (nVertex == m_VertexCnt)
	{
		// Vertex is unique.
		m_pVertices [m_VertexCnt] = pVertex;
		m_VertexCnt++;

		//printf ("\nADDED Unique Vertex!\n");
//		DisplayVertices ();
//		DecisionDisplay ();

	}
	else
	{
		delete [] pVertex;
		//printf ("\n+");
	}

}  /* CTableau :: AddVertex () */


std::string CTableau :: DisplayVertices ()
{
	int		nVertex;
	int		nParam;
    char buffer[1024];
    std::string result;
    
	result.append ("\n\n");
	for (nParam = 0; nParam < m_ParamCnt; nParam++)
	{
		snprintf (buffer, 1024, "%6s ", m_pVarLabels [nParam + 1]);
	    result.append(buffer);
	}
	result.append ("\n\n");

	for (nVertex = 0; nVertex < m_VertexCnt; nVertex++)
	{
		for (nParam = 0; nParam < m_ParamCnt; nParam++)
		{
			snprintf (buffer,1024, "%6.3lf ", m_pVertices [nVertex][nParam]);
		    result.append(buffer);
		}
		result.append ("\n");
	}
	return result;
}  /* CTableau :: DisplayVertices () */


BOOL CTableau :: GetVertex (int p_nVertex, double * p_pVertex, int p_VertexLength)
{
	int			nParam;

	if (p_VertexLength != m_ParamCnt)
		return FALSE;

	if (p_nVertex >= m_VertexCnt)
		return FALSE;

	for (nParam = 0; nParam < m_ParamCnt; nParam++)
		p_pVertex [nParam] = m_pVertices [p_nVertex][nParam];
	return TRUE;
}


