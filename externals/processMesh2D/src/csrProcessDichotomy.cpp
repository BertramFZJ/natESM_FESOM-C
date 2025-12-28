#include <stdio.h>
#include <stdlib.h>

#include "csrProcessDichotomy.h"

typedef struct
{
	int idSEQ;
	int idNEXT;
} gtypeNumSeqEl;

int csrDichotomyNumericalSequence(int n, int *numSeq)
{
	gtypeNumSeqEl *NSE = (gtypeNumSeqEl *)malloc(n * sizeof(gtypeNumSeqEl));
	int nInit = 0;

	int i;

	if(NSE == NULL)
	{ fprintf(stderr, "csrDichotomyNumericalSequence: ERROR --> Can't allocate memory <%d * gtypeNumSeqEl>\n", n); exit(0); }

	for(i=0; i<n; i++) NSE[i].idSEQ = NSE[i].idNEXT = -1;
	for(i=0; i<n; i++) numSeq[i] = -1;
	
	NSE[0].idSEQ = 0; NSE[0].idNEXT = n - 1;
	NSE[n-1].idSEQ =  1;
	nInit = 2;

	while(nInit != n)
	{
		int idCur = 0;
		
		while(1)
		{
			int idSet;

			if(idCur + 1 == NSE[idCur].idNEXT) idSet = -1;
			if(idCur + 2 == NSE[idCur].idNEXT) idSet = idCur + 1;
			if(idCur + 3 == NSE[idCur].idNEXT) idSet = idCur + 1;
			if(idCur + 3 <  NSE[idCur].idNEXT) idSet = (idCur + NSE[idCur].idNEXT) / 2;

			if(idSet != -1)
			{
				if(NSE[idSet].idSEQ != -1)
				{ 
					fprintf(stderr, "csrDichotomyNumericalSequence: ERROR --> NSE[idSet].idSEQ != -1\n");
					fprintf(stderr, "idSet = %d\n", idSet);
					exit(0); 
				}

				NSE[idSet].idSEQ = nInit; nInit += 1;
				NSE[idSet].idNEXT = NSE[idCur].idNEXT;
				NSE[idCur].idNEXT = idSet;
				idCur = idSet;
			}

			idCur = NSE[idCur].idNEXT;
			if(NSE[idCur].idNEXT == -1) break;
		} // while

		if(0) printf("nInit = %10d of %10d\n", nInit, n);		
	} // while

	for(i=0; i<n; i++) if(NSE[i].idSEQ == -1)
	{ fprintf(stderr, "csrDichotomyNumericalSequence: ERROR --> NSE[i].idSEQ == -1\n"); exit(0); }
	for(i=0; i<n; i++) numSeq[NSE[i].idSEQ] = i;
	for(i=0; i<n; i++) if(numSeq[i] == -1)
	{ fprintf(stderr, "csrDichotomyNumericalSequence: ERROR --> numSeq[i] == -1\n"); exit(0); }

	free(NSE);

	return 0;
}
