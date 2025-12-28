#ifndef _CSR_PREPROCESS_CM_
#define _CSR_PREPROCESS_CM_

int csrGraphRenameIndexCuthillMcKee(int n, int idFIRST, int *x, int *a, int *index);

int PseudoPeripheralCsrGraphVertex(int n, int *x, int *a, int printfFlag = 0);

int csrGraphRenameIndexKing(int n, int *x, int *a, int *index);

int csrGraphRenameIndexSloan(int N, int idFIRST, int *x, int *a, int *index);

#endif
