#include <R.h>
//#include <windows.h>
#include <stdio.h>
#include "Optimization.h"

COptimization_		g_Optimization;

main ()
{
	FILE *		pFile;
	char		szResponse [80];
	char		szFileName [80];
	int *		pArray;
//	HANDLE		hThread;

	Rprintf ("Optimization File: ");
	gets (szFileName);

	pFile = fopen (szFileName, "r");
	if (pFile == NULL)
	{
		error ("\nERROR: could not open file.\n\n");
		return FALSE;
	}

	if (! g_Optimization. ParseFile (pFile))
	{
		error ("\nERROR: unsuccessfully parsed file.\n\n");
		return FALSE;
	}

/*	hThread = GetCurrentThread ();
	SetThreadPriority (hThread, THREAD_PRIORITY_BELOW_NORMAL);
*/

	g_Optimization. CategorizeConstraints ();

	g_Optimization. GaussianElimination ();

	g_Optimization. EnumerateVertices ();

	g_Optimization. OutputOptimum ();

//	g_Optimization. Display (stdout);

//	CSymbolSet_ * g_Optimization. EliminateVariables ();

	//Rprintf ("Done.\n");	
	gets (szResponse);

	return TRUE;
}  /* main () */
