#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hybmeshdual2D.h"

typedef struct
{
	int iV;
	int E[2];
} t_Side;

static int PuzSort(int *P, int N);
static int PuzSort(int *P, int N)
{
	int i, j;
	
	for(i=0; i<N-1; i++)
		for(j=0; j<N-1-i; j++)
			if(P[j] > P[j+1])
			{
				int tmp;
				tmp = P[j+1];
				P[j+1] = P[j];
				P[j] = tmp;
			}

	return 0;
}

// 0 --> a == b
// 1 --> a > b
// (-1) --> a < b
static int compare_tS(t_Side a, t_Side b);
static int compare_tS(t_Side a, t_Side b)
{
	if(a.E[0] > b.E[0]) return 1;
	if(a.E[0] < b.E[0]) return (-1);

	if(a.E[1] > b.E[1]) return 1;
	if(a.E[1] < b.E[1]) return (-1);

	return 0;
}

static void wsort_tS(t_Side *A, int l, int r);
static void wsort_tS(t_Side *A, int l, int r)
{
  t_Side tmp;
  t_Side B = A[(l + r) / 2];
  int i = l, j = r;

  while( i <= j )
  {
 	while( compare_tS(A[i], B) == (-1) ) i++;
    while( compare_tS(A[j], B) == 1 ) j--;
    if( i <= j )
    {
      tmp = A[i];
      A[i] = A[j];
      A[j] = tmp;
      i++;
      j--;      
    }   
  }
  
  if( l < j ) wsort_tS(A, l, j);
  if( i < r ) wsort_tS(A, i, r);
}

int GenDualCSRForHybMesh2D(int Ne, int *EX, int *EA, int **X, int **A)
{
	int Ne3 = 0; // число треугольников
	int Ne4 = 0; // число четырехугольников	
	int NGT;
	t_Side *tS;
	int *x, *a;

	int i;

	printf("************** PROCEDURE GenDualCSRForHybMesh2D START  **************\n");

	// подсчет числа элементов сетки по типам (треугольник и четырехугольник)
	{
		int Nall;	

		for(i=0; i<Ne; i++)
		{
			int Nl = EX[i+1] - EX[i];
			if(Nl == 3) Ne3 += 1;
			if(Nl == 4) Ne4 += 1;			
		} // for i

		Nall = Ne3 + Ne4;
		
		if(Nall != Ne) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: # 1 #\n"); exit(0); }

		printf("\n");
		printf("MESH ELEMENTS DISTRIBUTION:\n");
		printf("     TRIANGLE: %15d\n", Ne3);
		printf("QUADRILATERAL: %15d\n", Ne4);		
		printf("\n");
	}
	// подсчет числа элементов сетки по типам (треугольник и четырехугольник)

	// составление общего списка граней сетки (с повторениями по смежным элементам)
	{
		int Nloc = 0;

		NGT = Ne3*3 + Ne4*4;
		
		// выделение памяти и начальная инициализация
		tS = NULL; tS = (t_Side *)malloc(NGT * sizeof(t_Side));
		if(tS == NULL) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: Can't allocate memory\n"); exit(0); }
		for(i=0; i<NGT; i++) { tS[i].iV = -1; tS[i].E[0] = tS[i].E[1] = -1; }
		
		// заполнение массива
		for(i=0; i<Ne; i++)
		{
			int ETYPE, *EPTR;
			int j;

			EPTR = EA + EX[i];
			ETYPE = EX[i+1] - EX[i];
			
			for(j=0; j<ETYPE; j++)
			{
				int E0 = EPTR[j];
				int E1 = EPTR[(j + 1) % ETYPE];				
				tS[Nloc].iV = i;
				if(E0 < E1) { tS[Nloc].E[0] = E0; tS[Nloc].E[1] = E1; }
				       else { tS[Nloc].E[0] = E1; tS[Nloc].E[1] = E0; }				
				Nloc += 1;			
			} // for j
		} // for i
		if(Nloc != NGT) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: # 2 #\n"); exit(0); }		
		printf("TOTAL SIDES NUMBER (DUPL): %18d\n", NGT);
	}
	// составление общего списка граней сетки (с повторениями по смежным элементам)

	// удаление граничных граней из общего списка (граней, у которых нет пары)
	{
		int Nloc = 0;

		// сортировка граней по возрастанию вершин в списках
		wsort_tS(tS, 0, NGT-1);		

		// фиксация (поиск) граней, у которых нет пары
		for(i=0; i<NGT; i++)
		{
			int get;

			if(i == NGT-1)
			{
				tS[i].iV = -1;
				break;
			}

			get = compare_tS(tS[i], tS[i+1]);
			if(get != 0)
			{
				tS[i].iV = -1;
			}
			else
			{
				i += 1;
			}
		} // for i
		
		// прореживание списка
		// в спсике в итоге оказываются только внутренние грани, упомянутые по 2 раза
		for(i=0; i<NGT; i++)
		{
			if(tS[i].iV != -1)
			{
				tS[Nloc] = tS[i];
				Nloc += 1;
			}
		} // for i
		
		if(Nloc%2 != 0) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: # 3 #\n"); exit(0); }
		printf("NUMBER OF INTERIOR SIDES : %18d\n", Nloc/2);
		printf("NUMBER OF BOUNDARY SIDES : %18d\n", NGT - Nloc);
		
		NGT = Nloc;
	}
	// удаление граничных граней из общего списка (граней, у которых нет пары)

	// формирование результирующего списка
	{
		// выделение памяти
		x = NULL; x = (int *)malloc((Ne + 1) * sizeof(int));
		if(x == NULL) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: Can't allocate memory\n"); exit(0); }
		for(i=0; i<=Ne; i++) x[i] = 0;

		a = NULL; a = (int *)malloc(NGT * sizeof(int));
		if(a == NULL) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: Can't allocate memory\n"); exit(0); }
		
		for(i=0; i<NGT; i++) x[tS[i].iV + 1] += 1;
		for(i=2; i<=Ne; i++) x[i] += x[i-1];
		if(x[Ne] != NGT) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: # 4 #\n"); exit(0); }

		for(i=0; i<NGT; i+=2)
		{
			int iL, iR;

			iL = tS[i  ].iV;
			iR = tS[i+1].iV;

			a[x[iL]] = iR; x[iL] += 1;
			a[x[iR]] = iL; x[iR] += 1;
		} // for i

		if(x[Ne] != x[Ne-1]) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: # 5 #\n"); exit(0); }
		for(i=Ne-1; i>0; i--) x[i] = x[i-1]; x[0] = 0;
		
		// сортировка элементов в локальных списках связей по возрастанию
		for(i=0; i<Ne; i++)
		{
			int Nl = x[i+1] - x[i];
			int *ptr = a + x[i];
			
			PuzSort(ptr, Nl);
		} // for i

		printf("\n");
		printf("CREATE RESULTS CSR ARRAYS: \n");		
	}
	// формирование результирующего списка

	free(tS);

	// статистика число связей <--> число элементов
	{
		int NN[5] = {0, 0, 0, 0, 0};

		for(i=0; i<Ne; i++)
		{
			int Nl = x[i+1] - x[i];
			NN[Nl] += 1;			
		} // for i

		if(NN[0] != 0) { fprintf(stderr, "<GenDualCSRForHybMesh2D> ERROR: # 6 #\n"); exit(0); }
		for(i=0; i<5; i++) if(NN[i] != 0) printf("%d LINKS: %8d ELEMENTS\n", i, NN[i]);		
	}

	(*X) = x;
	(*A) = a;

	printf("\n");
	printf("************** PROCEDURE GenDualCSRForHybMesh2D FINISH **************\n");

	return 0;
}

int GenDualCSRNodalForHybMesh2D(int Np, int Ne, int *EX, int *EA, int **X, int **A)
{
	int *NX, *NA;
	int *RX, *RA;
	int *FLAG;

	int i, j, k;

	  NX = (int *)malloc((Np + 1) * sizeof(int)); if(NX == NULL) exit(0);
	  NA = (int *)malloc(EX[Ne] * sizeof(int)); if(NA == NULL) exit(0);
	  RX = (int *)malloc((Ne + 1) * sizeof(int)); if(RX == NULL) exit(0);
	  RA = (int *)malloc(Ne * 20 * sizeof(int)); if(RA == NULL) exit(0);
	FLAG = (int *)malloc(Ne * sizeof(int)); if(FLAG == NULL) exit(0);

	for(i=0; i<=Np; i++) NX[i] = 0;

	for(i=0; i<Ne; i++)
		for(j=EX[i]; j<EX[i+1]; j++)
			NX[EA[j] + 1] += 1;

	for(i=2; i<=Np; i++) NX[i] += NX[i - 1];
	if(NX[Np] != EX[Ne]) { fprintf(stderr, "ERROR 111\n"); exit(0); }

	for(i=0; i<Ne; i++)
		for(j=EX[i]; j<EX[i+1]; j++)
		{
			NA[NX[EA[j]]] = i;
			NX[EA[j]] += 1;
		}
	if(NX[Np - 1] != NX[Np]) { fprintf(stderr, "ERROR 222\n"); exit(0); }
	for(i=Np-1; i>0; i--) NX[i] = NX[i-1]; NX[0] = 0;

	RX[0] = 0;
	for(i=0; i<Ne; i++) FLAG[i] = 0;

	for(i=0; i<Ne; i++)
	{
		int LIST[100];
		int NL = 0;
		int *PTR = EA + EX[i];
		int NP = EX[i+1] - EX[i];

		// printf("i = %d\n", i);

		NL = 0;
		for(j=0; j<NP; j++)
		{
			for(k=NX[PTR[j]]; k<NX[PTR[j] + 1]; k++)
				if( (NA[k] != i) && (FLAG[NA[k]] != 1) )
				{
					LIST[NL] = NA[k];
					FLAG[NA[k]] = 1;
					NL += 1;
				}
		} // for j

		if(RX[i] + NL > Ne * 20) { fprintf(stderr, "ERROR 333\n"); exit(0); }

		for(j=0; j<NL; j++)
		{
			RA[RX[i] + j] = LIST[j];
			FLAG[LIST[j]] = 0;
		} // for j
		RX[i + 1] = RX[i] + NL;
	} // for i

	free(NX); free(NA); free(FLAG);
	RA = (int *)realloc(RA, RX[Ne] * sizeof(int));

	(*X) = RX;
	(*A) = RA;

	return 0;
}
