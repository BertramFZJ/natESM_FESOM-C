#ifndef _TYPE_MeshHybQuadTri2D_
typedef struct
{
	int NP; // число сеточных узлов
	int NE; // число сеточных элементов
	int NE3; // число сеточных треугольников
	int NE4; // число сеточных четырехугольников
	int NBC; // число выделенных граничных поверхностей
	int NBE; // суммарное число ребер на выделенных граничных поверхностях

	double *CRD; // указатель на массив с парами координат сеточных узлов

	int *XE; // указатель на массив со стартовыми смещениями троек и четверок идентификаторов вершин сеточных элементов
	int *AE; // указатель на массив с тройками и четверками идентификаторов вершин сеточных элементов

	int *BE; // указатель на массив с тройками целых чисел, описывающих ребра выделенных граничных поверхностей
	         // BE[i*3 + 0] = идентификатор сеточного элемента, которому принадлежит ребро [0, NE-1]
	         // BE[i*3 + 1] = идентификатор грани сеточного элемента [0, 2/3]
	         // BE[i*3 + 2] = идентификатор граничной поверхности [0, NBC-1]

	void *PTR; // глобальный указатель на динамически выделенный массив для записи данных
	long  MEM; // размер выделенной памяти (байт)
} t_MeshHybQuadTri2D;
#define _TYPE_MeshHybQuadTri2D_
#endif

int IOHM2D_FreeMeshHybQuadTri2DStructure(t_MeshHybQuadTri2D *HM2D);

int IOHM2D_WriteMeshToFileText(char *fname, t_MeshHybQuadTri2D HM2D);
int IOHM2D_WriteMeshToFileBinary(char *fname, t_MeshHybQuadTri2D HM2D);

int IOHM2D_ReadMeshFromFileText(char *fname, t_MeshHybQuadTri2D *HM2D);
int IOHM2D_ReadMeshFromFileBinary(char *fname, t_MeshHybQuadTri2D *HM2D);

int IOHM2D_TestHybQuadTri2DMesh(t_MeshHybQuadTri2D HM2D, FILE *stream);

int IOHM2D_PlotHybQuadTri2DMesh(char *fname, t_MeshHybQuadTri2D HM2D);

int IOHM2D_PlotHybQuadTri2DMeshInterior(char *fname, 
										int NP, double *CRD,
										int NE, int *EX, int *EA);

int IOHM2D_PlotHybQuadTri3DMeshInterior(char *fname, 
										int NP, double *CRD,
										int NE, int *EX, int *EA);

int IOHM2D_PlotHybQuadTri3DMeshFunc(char *fname, 
									int NP, double *CRD,
									int NE, int *EX, int *EA,									
									int nF, double *func,
									char *varNames);

int IOHM2D_PlotHybQuadTri2DMeshFunc(char *fname, 
									int NP, double *CRD,
									int NE, int *EX, int *EA,									
									int nF, double *func,
									char *varNames);

int IOHM2D_PlotHybQuadTri3DMeshFuncVertex(char *fname, 
									      int NP, double *CRD,
									      int NE, int *EX, int *EA,									
									      int nF, double *func,
									      char *varNames);

int IOHM2D_PlotHybQuadTri2DMeshPart(int nP, double *c, int nE, int *x, int *a, int *part, char *fname);
