#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "ioHM2D.h"

int IOHM2D_FreeMeshHybQuadTri2DStructure(t_MeshHybQuadTri2D *HM2D)
{
	free(HM2D->PTR); HM2D->PTR = NULL; HM2D->MEM = 0;
	HM2D->AE = HM2D->BE = HM2D->XE = NULL; HM2D->CRD = NULL;
	HM2D->NBC = HM2D->NBE = HM2D->NE = HM2D->NE3 = HM2D->NE4 = HM2D->NP = 0;

	return 0;
}

int IOHM2D_WriteMeshToFileText(char *fname, t_MeshHybQuadTri2D HM2D)
{
	int i, j;
	FILE *file = NULL;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_WriteMeshToFileText> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	fprintf(file, "%d %d %d %d %d %d\n", HM2D.NP, HM2D.NE, HM2D.NE3, HM2D.NE4, HM2D.NBC, HM2D.NBE);

	for(i=0; i<HM2D.NP; i++) fprintf(file, "%.12E %.12E\n", HM2D.CRD[i*2 + 0], HM2D.CRD[i*2 + 1]);

	for(i=0; i<HM2D.NE; i++)
	{
		int OFFSET = HM2D.XE[i];
		int TYPE = HM2D.XE[i+1] - OFFSET;
		
		fprintf(file, "%d %d", TYPE, HM2D.AE[OFFSET]);
		for(j=1; j<TYPE; j++) fprintf(file, " %d", HM2D.AE[OFFSET + j]);
		fprintf(file, "\n");
	} // for i

	for(i=0; i<HM2D.NBE; i++) fprintf(file, "%d %d %d\n", HM2D.BE[i*3 + 0], HM2D.BE[i*3 + 1], HM2D.BE[i*3 + 2]);

	fclose(file);

	printf("\n");
	printf("CREATE HYBRID MESH 2D DATA FILE \"%s\"\n", fname);
	
	return 0;
}

int IOHM2D_WriteMeshToFileBinary(char *fname, t_MeshHybQuadTri2D HM2D)
{
	int get;
	FILE *file = NULL;

	file = fopen(fname, "wb");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	get = (int)fwrite(&HM2D.NP , sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	get = (int)fwrite(&HM2D.NE , sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	get = (int)fwrite(&HM2D.NE3, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	get = (int)fwrite(&HM2D.NE4, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	get = (int)fwrite(&HM2D.NBC, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	get = (int)fwrite(&HM2D.NBE, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }

	get = (int)fwrite(HM2D.CRD, sizeof(double), 2 * HM2D.NP, file); if(get != 2 * HM2D.NP) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }

	get = (int)fwrite(HM2D.XE, sizeof(int), HM2D.NE + 1     , file); if(get != HM2D.NE + 1     ) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	get = (int)fwrite(HM2D.AE, sizeof(int), HM2D.XE[HM2D.NE], file); if(get != HM2D.XE[HM2D.NE]) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }

	get = (int)fwrite(HM2D.BE, sizeof(int), HM2D.NBE * 3, file); if(get != HM2D.NBE * 3) { fprintf(stderr, "<IOHM2D_WriteMeshToFileBinary> ERROR: CAN'T WRITE DATA TO FILE %s\n", fname); exit(0); }
	
	fclose(file);

	printf("\n");
	printf("CREATE HYBRID MESH 2D DATA FILE \"%s\"\n", fname);
	
	return 0;
}

int IOHM2D_ReadMeshFromFileText(char *fname, t_MeshHybQuadTri2D *HM2D)
{
	int i, j;
	char line[1024], *oldstr, *newstr;
	FILE *file = NULL;

	file = fopen(fname, "r");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileText> ERROR: CAN'T OPEN FILE %s\n", fname); exit(0); }
	
	fgets(line, 500, file); oldstr = line; newstr = NULL;
	HM2D->NP  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
	HM2D->NE  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
	HM2D->NE3 = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
	HM2D->NE4 = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
	HM2D->NBC = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
	HM2D->NBE = (int)strtol(oldstr, &newstr, 10);

	HM2D->MEM = (long)((HM2D->NP * 2) * sizeof(double) + (HM2D->NE3 * 3 + HM2D->NE4 * 4 + HM2D->NBE * 3 + HM2D->NE + 1) * sizeof(int));
	HM2D->PTR = NULL; HM2D->PTR = (void *)malloc(HM2D->MEM * sizeof(char));
	if(HM2D->PTR == NULL) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileText> ERROR: CAN'T ALLOCATE MEMORY\n"); exit(0); }
	HM2D->CRD = (double *)HM2D->PTR;
	 HM2D->XE = (int *)(HM2D->CRD + HM2D->NP * 2);
	 HM2D->AE = HM2D->XE + HM2D->NE + 1;
	 HM2D->BE = HM2D->AE + HM2D->NE3 * 3 + HM2D->NE4 * 4;

	for(i=0; i<HM2D->NP; i++)
	{
		fgets(line, 500, file); oldstr = line; newstr = NULL;
		HM2D->CRD[i*2 + 0] = (double)strtod(oldstr, &newstr); oldstr = newstr;
		HM2D->CRD[i*2 + 1] = (double)strtod(oldstr, &newstr);
	} // for i

	HM2D->XE[0] = 0;
	for(i=0; i<HM2D->NE; i++)
	{
		int OFFSET = HM2D->XE[i];
		int TYPE;
		
		fgets(line, 500, file); oldstr = line; newstr = NULL;
		TYPE = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
		HM2D->XE[i + 1] = OFFSET + TYPE;
		for(j=0; j<TYPE; j++) 
		{
			HM2D->AE[OFFSET + j] = (int)strtol(oldstr, &newstr, 10); 
			oldstr = newstr;
		} // for j		
	} // for i
	if(HM2D->XE[HM2D->NE] != HM2D->NE3 * 3 + HM2D->NE4 * 4) 
	{ fprintf(stderr, "<IOHM2D_ReadMeshFromFileText> ERROR: XE[NE] != NE3*3 + NE4*4\n"); exit(0); }

	for(i=0; i<HM2D->NBE; i++)
	{
		fgets(line, 500, file); oldstr = line; newstr = NULL;
		HM2D->BE[i*3 + 0] = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
		HM2D->BE[i*3 + 1] = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
		HM2D->BE[i*3 + 2] = (int)strtol(oldstr, &newstr, 10);
	} // for i

	fclose(file);

	printf("\n");
	printf("READ HYBRID MESH 2D TOPOLOGY FROM DATA FILE \"%s\"\n", fname);
	
	return 0;
}

int IOHM2D_ReadMeshFromFileBinary(char *fname, t_MeshHybQuadTri2D *HM2D)
{
	int get;
	FILE *file = NULL;

	file = fopen(fname, "rb");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T OPEN FILE %s\n", fname); exit(0); }

	get = (int)fread(&HM2D->NP , sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
	get = (int)fread(&HM2D->NE , sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
	get = (int)fread(&HM2D->NE3, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
	get = (int)fread(&HM2D->NE4, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
	get = (int)fread(&HM2D->NBC, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
	get = (int)fread(&HM2D->NBE, sizeof(int), 1, file); if(get != 1) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
		
	HM2D->MEM = (long)((HM2D->NP * 2) * sizeof(double) + (HM2D->NE3 * 3 + HM2D->NE4 * 4 + HM2D->NBE * 3 + HM2D->NE + 1) * sizeof(int));
	HM2D->PTR = NULL; HM2D->PTR = (void *)malloc(HM2D->MEM * sizeof(char));
	if(HM2D->PTR == NULL) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T ALLOCATE MEMORY\n"); exit(0); }
	HM2D->CRD = (double *)HM2D->PTR;
	 HM2D->XE = (int *)(HM2D->CRD + HM2D->NP * 2);
	 HM2D->AE = HM2D->XE + HM2D->NE + 1;
	 HM2D->BE = HM2D->AE + HM2D->NE3 * 3 + HM2D->NE4 * 4;
	 
	 HM2D->CRD = (double *)malloc(HM2D->NP * 2 * sizeof(double));
	get = (int)fread(HM2D->CRD, sizeof(double), 2 * HM2D->NP, file); if(get != 2 * HM2D->NP) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }

	get = (int)fread(HM2D->XE, sizeof(int), HM2D->NE + 1      , file); if(get != HM2D->NE + 1      ) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }
	get = (int)fread(HM2D->AE, sizeof(int), HM2D->XE[HM2D->NE], file); if(get != HM2D->XE[HM2D->NE]) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }

	get = (int)fread(HM2D->BE, sizeof(int), HM2D->NBE * 3, file); if(get != HM2D->NBE * 3) { fprintf(stderr, "<IOHM2D_ReadMeshFromFileBinary> ERROR: CAN'T READ DATA FROM FILE %s\n", fname); exit(0); }

	fclose(file);

	printf("\n");
	printf("READ HYBRID MESH 2D TOPOLOGY FROM DATA FILE \"%s\"\n", fname);
	
	return 0;
}

static double SignTriangleSquare2D(double *A, double *B, double *C);
static double SignTriangleSquare2D(double *A, double *B, double *C)
{
	double AB[2] = {B[0] - A[0], B[1] - A[1]};
	double AC[2] = {C[0] - A[0], C[1] - A[1]};
	double SQ = 0.5 * (AB[0] * AC[1] - AB[1] * AC[0]);
	return SQ; 
}

static double SignQuadSquare2D(double *A, double *B, double *C, double *D);
static double SignQuadSquare2D(double *A, double *B, double *C, double *D)
{
	double SQ = SignTriangleSquare2D(A, B, C) + SignTriangleSquare2D(A, C, D);
	return SQ;
}
	
int IOHM2D_TestHybQuadTri2DMesh(t_MeshHybQuadTri2D HM2D, FILE *stream)
{
	fprintf(stream, "\n");
	fprintf(stream, "***** TEST HYBRID MESH TOPOLOGY *****\n");

	// ������������ ����� �����
	{
		fprintf(stream, "\n");
		fprintf(stream, "NUMBER OF MESH NODES: %8d\n", HM2D.NP);
	}
	// ������������ ����� �����

	// ������������ �������� ���������
	{
		int i;
		int init3 = 0;
		int init4 = 0;
		double SQ3[2], SQ4[2];
		double SQTOTAL, SQTRI, SQQUAD;

		SQTOTAL = SQTRI = SQQUAD = 0.0;
		for(i=0; i<HM2D.NE; i++)
		{
			int OFFSET = HM2D.XE[i];
			int TYPE = HM2D.XE[i+1] - OFFSET;
			int *PTR = HM2D.AE + OFFSET;

			if(TYPE == 3)
			{
				double SQ = SignTriangleSquare2D(HM2D.CRD + PTR[0]*2, HM2D.CRD + PTR[1]*2, HM2D.CRD + PTR[2]*2);

				if(init3 == 0) { SQ3[0] = SQ3[1] = SQ; init3 = 1; }
				else { if(SQ3[0] > SQ) SQ3[0] = SQ;	if(SQ3[1] < SQ) SQ3[1] = SQ; }
				
				SQTOTAL += SQ; 
				  SQTRI += SQ;
			}

			if(TYPE == 4)
			{
				double SQ = SignQuadSquare2D(HM2D.CRD + PTR[0]*2, HM2D.CRD + PTR[1]*2, HM2D.CRD + PTR[2]*2, HM2D.CRD + PTR[3]*2);

				if(init4 == 0) { SQ4[0] = SQ4[1] = SQ; init4 = 1; }
				else { if(SQ4[0] > SQ) SQ4[0] = SQ;	if(SQ4[1] < SQ) SQ4[1] = SQ; }

				SQTOTAL += SQ; 
				 SQQUAD += SQ;
			}
		} // for i

		fprintf(stream, "\n");
		fprintf(stream, " NUMBER OF MESH ELEMENTS: %8d\n", HM2D.NE);
		fprintf(stream, "     NUMBER OF TRIANGLES: %8d\n", HM2D.NE3);
		fprintf(stream, "NUMBER OF QUADRILATERALS: %8d\n", HM2D.NE4);
		if(HM2D.NE3 != 0) fprintf(stream, "        SQUARE TRIANGLES: %.6E [%.6E, %.6E]\n", SQTRI, SQ3[0], SQ3[1]);
		if(HM2D.NE4 != 0) fprintf(stream, "   SQUARE QUADRILATERALS: %.6E [%.6E, %.6E]\n", SQQUAD, SQ4[0], SQ4[1]);
		fprintf(stream, "            TOTAL SQUARE: %13g (%.12E)\n", SQTOTAL, SQTOTAL);
	}
	// ������������ �������� ���������

	// ������������ �������
	{
		int i;
		
		double *BNDPAR  = (double *)malloc(HM2D.NBE * 3 * sizeof(double));
		   int *BNDPARI = (int    *)malloc(HM2D.NBE * 1 * sizeof(   int));
		double TOTALRES[3] = {0.0, 0.0, 0.0};
		
		if(BNDPAR  == NULL) { fprintf(stderr, "<IOHM2D_TestHybQuadTri2DMesh> ERROR: CAN'T ALLOCATE MEMORY\n"); exit(0); } 
		memset( BNDPAR, 0, HM2D.NBE * 3 * sizeof(double));
		if(BNDPARI == NULL) { fprintf(stderr, "<IOHM2D_TestHybQuadTri2DMesh> ERROR: CAN'T ALLOCATE MEMORY\n"); exit(0); } 
		memset(BNDPARI, 0, HM2D.NBE * 1 * sizeof(int   ));

		for(i=0; i<HM2D.NBE; i++)
		{
			int *PTRF = HM2D.BE + i*3;
			int *PTRE = HM2D.AE + HM2D.XE[PTRF[0]];
			int  TYPE = HM2D.XE[PTRF[0] + 1] - HM2D.XE[PTRF[0]];
			double *PTRA = HM2D.CRD + PTRE[ PTRF[1]          ]*2;
			double *PTRB = HM2D.CRD + PTRE[(PTRF[1] + 1)%TYPE]*2;
			double N[2] = {PTRB[0] - PTRA[0], PTRB[1] - PTRA[1]};
			double L = sqrt(N[0]*N[0] + N[1]*N[1]);

			BNDPAR[PTRF[2]*3 + 0] +=      L;
			BNDPAR[PTRF[2]*3 + 1] += - N[1];
			BNDPAR[PTRF[2]*3 + 2] +=   N[0];
			BNDPARI[PTRF[2]] += 1;
		} // for i

		for(i=0; i<HM2D.NBC; i++)
		{
			TOTALRES[0] += BNDPAR[i*3 + 0];
			TOTALRES[1] += BNDPAR[i*3 + 1];
			TOTALRES[2] += BNDPAR[i*3 + 2];
		} // for i

		fprintf(stream, "\n");
		fprintf(stream, "         NUMBER OF BOUNDARY FACES: %8d\n", HM2D.NBC);
		fprintf(stream, "TOTAL NUMBER OF BOUNDARY SEGMENTS: %8d\n", HM2D.NBE);

		fprintf(stream, "FACE TAG             L        VECTORX        VECTORY       NE\n");
		for(i=0; i<HM2D.NBC; i++) 
			fprintf(stream, "      %2d %.6E %14.6E %14.6E %8d\n", i, BNDPAR[i*3 + 0], BNDPAR[i*3 + 1], BNDPAR[i*3 + 2], BNDPARI[i]);
		fprintf(stream, "   TOTAL %.6E %14.6E %14.6E %8d\n", TOTALRES[0], TOTALRES[1], TOTALRES[2], HM2D.NBE);

		free(BNDPAR); free(BNDPARI);
	}
	// ������������ �������

	fprintf(stream, "\n");
	fprintf(stream, "*************************************\n");

	return 0;
}

int IOHM2D_PlotHybQuadTri2DMesh(char *fname, t_MeshHybQuadTri2D HM2D)
{
	int i, j;
	int *VID = NULL;
	FILE *file = NULL;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMesh> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	// ������ ������������ ������ ����� �����
	fprintf(file, "VARIABLES= \"X\",\"Y\"\n");
	// ������ ������������ ������ ����� �����


	// ������ ��������� ����� � �����
	fprintf(file, "ZONE T=\"MESH <GLOBAL>\"\n");
	
	if(HM2D.NE4 == 0) fprintf(file, "F=FEPOINT,      ET=TRIANGLE, N=%d E=%d\n", HM2D.NP, HM2D.NE);
	             else fprintf(file, "F=FEPOINT, ET=QUADRILATERAL, N=%d E=%d\n", HM2D.NP, HM2D.NE);

	for(i=0; i<HM2D.NP; i++) fprintf(file, "%.12E %.12E\n", HM2D.CRD[i*2 + 0], HM2D.CRD[i*2 + 1]);

	if(HM2D.NE4 == 0) for(i=0; i<HM2D.NE; i++) fprintf(file, "%d %d %d\n", HM2D.AE[i*3 + 0] + 1, HM2D.AE[i*3 + 1] + 1, HM2D.AE[i*3 + 2] + 1);
	if(HM2D.NE4 != 0) for(i=0; i<HM2D.NE; i++)
	{
		int TYPE = HM2D.XE[i+1] - HM2D.XE[i];
		int *PTR = HM2D.AE + HM2D.XE[i];
		int V4[4];

		for(j=0; j<4; j++)
		{
			if(j < TYPE) V4[j] = PTR[j] + 1;
			        else V4[j] = V4[j-1];
		} // for j

		fprintf(file, "%d %d %d %d\n", V4[0], V4[1], V4[2], V4[3]);
	} // for i
	// ������ ��������� ����� � �����

	VID = (int *)malloc(HM2D.NP * sizeof(int)); 
	if(VID == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMesh> ERROR: CAN'T ALLOCATE MEMORY\n"); exit(0); }
	for(i=0; i<HM2D.NP; i++) VID[i] = -1;

	// ������ �������
	{
		int *BE = (int *)malloc(HM2D.NBE * 3 * sizeof(int));
		if(BE == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMesh> ERROR: CAN'T ALLOCATE MEMORY\n"); exit(0); }

		for(i=0; i<HM2D.NBE; i++)
		{
			int *PTRF = HM2D.BE + i*3;
			int *PTRE = HM2D.AE + HM2D.XE[PTRF[0]];
			int  TYPE = HM2D.XE[PTRF[0] + 1] - HM2D.XE[PTRF[0]];
			
			BE[i*3 + 0] = PTRE[ PTRF[1]          ]; 
			BE[i*3 + 1] = PTRE[(PTRF[1] + 1)%TYPE];
			BE[i*3 + 2] = PTRF[2];
		} // for i

		for(i=0; i<HM2D.NBC; i++)
		{
			int NVL = 0;
			int NEL = 0;

			for(j=0; j<HM2D.NBE; j++) if(BE[j*3 + 2] == i) { VID[BE[j*3 + 0]] = 0; VID[BE[j*3 + 1]] = 0; NEL += 1; }
			for(j=0; j<HM2D.NP; j++) if(VID[j] == 0) VID[j] = NVL + 1, NVL += 1;

			fprintf(file, "ZONE T=\"BOUNDARY <TAG = %d>\"\n", i);
			fprintf(file, "F=FEPOINT, ET=TRIANGLE, N=%d E=%d\n", NVL, NEL);
			for(j=0; j<HM2D.NP; j++) if(VID[j] != -1) fprintf(file, "%.12E %.12E\n", HM2D.CRD[j*2 + 0], HM2D.CRD[j*2 + 1]);
			for(j=0; j<HM2D.NBE; j++) if(BE[j*3 + 2] == i) fprintf(file, "%d %d %d\n", VID[BE[j*3 + 0]], VID[BE[j*3 + 1]], VID[BE[j*3 + 1]]);

			for(j=0; j<HM2D.NP; j++) VID[j] = -1;
		} // for i

		free(BE);
	}
	// ������ �������

	// ������ ����������� � ��������������� �������� � ������ ��������� ���������
	if( (HM2D.NE3 != 0) && (HM2D.NE4 != 0) )
	{
		int NVL = 0;
		int NEL = 0;

		for(i=0; i<HM2D.NE; i++)
		{
			int OFFSET = HM2D.XE[i];
			int TYPE = HM2D.XE[i+1] - OFFSET;
			int *PTR = HM2D.AE + OFFSET;

			if(TYPE == 3) 
			{
				for(j=0; j<3; j++) VID[PTR[j]] = 0;
				NEL += 1;
			} // if
		} // for i
		if(NEL != HM2D.NE3) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMesh> ERROR: NEL != HM2D.NE3\n"); exit(0); }
		for(j=0; j<HM2D.NP; j++) if(VID[j] == 0) VID[j] = NVL + 1, NVL += 1;

		fprintf(file, "ZONE T=\"TRIANGLE SUBMESH\"\n");
		fprintf(file, "F=FEPOINT, ET=TRIANGLE, N=%d E=%d\n", NVL, NEL);
		for(j=0; j<HM2D.NP; j++) if(VID[j] != -1) fprintf(file, "%.12E %.12E\n", HM2D.CRD[j*2 + 0], HM2D.CRD[j*2 + 1]);
		for(j=0; j<HM2D.NE; j++) if(HM2D.XE[j+1] - HM2D.XE[j] == 3)
		{
			int *PTR = HM2D.AE + HM2D.XE[j];
			fprintf(file, "%d %d %d\n", VID[PTR[0]], VID[PTR[1]], VID[PTR[2]]);
		} // for j

		for(j=0; j<HM2D.NP; j++) VID[j] = -1;
		NVL = NEL = 0;

		for(i=0; i<HM2D.NE; i++)
		{
			int OFFSET = HM2D.XE[i];
			int TYPE = HM2D.XE[i+1] - OFFSET;
			int *PTR = HM2D.AE + OFFSET;

			if(TYPE == 4) 
			{
				for(j=0; j<4; j++) VID[PTR[j]] = 0;
				NEL += 1;
			} // if
		} // for i
		if(NEL != HM2D.NE4) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMesh> ERROR: NEL != HM2D.NE4\n"); exit(0); }
		for(j=0; j<HM2D.NP; j++) if(VID[j] == 0) VID[j] = NVL + 1, NVL += 1;

		fprintf(file, "ZONE T=\"QUADRILATERAL SUBMESH\"\n");
		fprintf(file, "F=FEPOINT, ET=QUADRILATERAL, N=%d E=%d\n", NVL, NEL);
		for(j=0; j<HM2D.NP; j++) if(VID[j] != -1) fprintf(file, "%.12E %.12E\n", HM2D.CRD[j*2 + 0], HM2D.CRD[j*2 + 1]);
		for(j=0; j<HM2D.NE; j++) if(HM2D.XE[j+1] - HM2D.XE[j] == 4)
		{
			int *PTR = HM2D.AE + HM2D.XE[j];
			fprintf(file, "%d %d %d %d\n", VID[PTR[0]], VID[PTR[1]], VID[PTR[2]], VID[PTR[3]]);
		} // for j
	}
	// ������ ����������� � ��������������� �������� � ������ ��������� ���������

	free(VID);

	fclose(file);

	printf("\n");
	printf("CREATE HYBRID MESH 2D TOPOLOGY TECPLOT VISUAL FILE \"%s\"\n", fname);

	return 0;
}

int IOHM2D_PlotHybQuadTri2DMeshInterior(char *fname, 
										int NP, double *CRD,
										int NE, int *EX, int *EA)
{
	int NE4 = 0;

	int i, j;
	FILE *file = NULL;

	for(i=0; i<NE; i++) if(EX[i+1] - EX[i] == 4) NE4 += 1;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMeshInterior> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	// ������ ������������ ������ ����� �����
	fprintf(file, "VARIABLES= \"X\",\"Y\"\n");
	// ������ ������������ ������ ����� �����


	// ������ ��������� ����� � �����
	fprintf(file, "ZONE T=\"MESH <GLOBAL>\"\n");
	
	if(NE4 == 0) fprintf(file, "F=FEPOINT,      ET=TRIANGLE, N=%d E=%d\n", NP, NE);
	        else fprintf(file, "F=FEPOINT, ET=QUADRILATERAL, N=%d E=%d\n", NP, NE);

	for(i=0; i<NP; i++) fprintf(file, "%18.10E %18.10E\n", CRD[i*2 + 0], CRD[i*2 + 1]);

	if(NE4 == 0) for(i=0; i<NE; i++) fprintf(file, "%d %d %d\n", EA[i*3 + 0] + 1, EA[i*3 + 1] + 1, EA[i*3 + 2] + 1);
	if(NE4 != 0) for(i=0; i<NE; i++)
	{
		int TYPE = EX[i+1] - EX[i];
		int *PTR = EA + EX[i];
		int V4[4];

		for(j=0; j<4; j++)
		{
			if(j < TYPE) V4[j] = PTR[j] + 1;
			        else V4[j] = V4[j-1];
		} // for j

		fprintf(file, "%d %d %d %d\n", V4[0], V4[1], V4[2], V4[3]);
	} // for i
	// ������ ��������� ����� � �����

	fclose(file);

	printf("\n");
	printf("CREATE HYBRID MESH 2D INTERIOR TOPOLOGY TECPLOT VISUAL FILE \"%s\"\n", fname);

	return 0;
}

int IOHM2D_PlotHybQuadTri3DMeshInterior(char *fname, 
										int NP, double *CRD,
										int NE, int *EX, int *EA)
{
	int NE4 = 0;

	int i, j;
	FILE *file = NULL;

	for(i=0; i<NE; i++) if(EX[i+1] - EX[i] == 4) NE4 += 1;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri3DMeshInterior> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	// ������ ������������ ������ ����� �����
	fprintf(file, "VARIABLES= \"X\",\"Y\",\"Z\"\n");
	// ������ ������������ ������ ����� �����


	// ������ ��������� ����� � �����
	fprintf(file, "ZONE T=\"MESH <GLOBAL>\"\n");
	
	if(NE4 == 0) fprintf(file, "F=FEPOINT,      ET=TRIANGLE, N=%d E=%d\n", NP, NE);
	        else fprintf(file, "F=FEPOINT, ET=QUADRILATERAL, N=%d E=%d\n", NP, NE);

	for(i=0; i<NP; i++) fprintf(file, "%18.10E %18.10E %18.10E\n", CRD[i*3 + 0], CRD[i*3 + 1], CRD[i*3 + 2]);

	if(NE4 == 0) for(i=0; i<NE; i++) fprintf(file, "%d %d %d\n", EA[i*3 + 0] + 1, EA[i*3 + 1] + 1, EA[i*3 + 2] + 1);
	if(NE4 != 0) for(i=0; i<NE; i++)
	{
		int TYPE = EX[i+1] - EX[i];
		int *PTR = EA + EX[i];
		int V4[4];

		for(j=0; j<4; j++)
		{
			if(j < TYPE) V4[j] = PTR[j] + 1;
			        else V4[j] = V4[j-1];
		} // for j

		fprintf(file, "%d %d %d %d\n", V4[0], V4[1], V4[2], V4[3]);
	} // for i
	// ������ ��������� ����� � �����

	fclose(file);

	printf("\n");
	printf("CREATE SURFACE HYBRID MESH 3D INTERIOR TOPOLOGY TECPLOT VISUAL FILE \"%s\"\n", fname);

	return 0;
}

int IOHM2D_PlotHybQuadTri3DMeshFunc(char *fname, 
									int NP, double *CRD,
									int NE, int *EX, int *EA,									
									int nF, double *func,
									char *varNames)
{
	int NE4 = 0;

	int i, j;
	FILE *file = NULL;

	for(i=0; i<NE; i++) if(EX[i+1] - EX[i] == 4) NE4 += 1;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri3DMeshFunc> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	// ������ ������������ ������ ����� �����
	fprintf(file, "VARIABLES= \"X\",\"Y\",\"Z\"");
	if(varNames == NULL) for(j=0; j<nF; j++) fprintf(file, ",\"VAR%d\"", j);
	else fprintf(file, "%s", varNames);
	fprintf(file, "\n");
	// ������ ������������ ������ ����� �����

	// ������ ��������� ����� � �����
	fprintf(file, "ZONE\n");

	if(NE4 == 0) fprintf(file, "F=FEBLOCK,      ET=TRIANGLE, N=%d E=%d VARLOCATION=([4", NP, NE);
	        else fprintf(file, "F=FEBLOCK, ET=QUADRILATERAL, N=%d E=%d VARLOCATION=([4", NP, NE);
	for(j=1; j<nF; j++) fprintf(file, ",%d", 4+j);
	fprintf(file, "]=CELLCENTERED)\n");

	for(i=0; i<NP; i++) { fprintf(file, "%g ", CRD[i*3 + 0]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	for(i=0; i<NP; i++) { fprintf(file, "%g ", CRD[i*3 + 1]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	for(i=0; i<NP; i++) { fprintf(file, "%g ", CRD[i*3 + 2]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	for(j=0; j<nF; j++)
	for(i=0; i<NE; i++) { fprintf(file, "%g ", func[i*nF + j]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	
	if(NE4 == 0) for(i=0; i<NE; i++) fprintf(file, "%d %d %d\n", EA[i*3 + 0] + 1, EA[i*3 + 1] + 1, EA[i*3 + 2] + 1);
	if(NE4 != 0) for(i=0; i<NE; i++)
	{
		int TYPE = EX[i+1] - EX[i];
		int *PTR = EA + EX[i];
		int V4[4];

		for(j=0; j<4; j++)
		{
			if(j < TYPE) V4[j] = PTR[j] + 1;
			        else V4[j] = V4[j-1];
		} // for j

		fprintf(file, "%d %d %d %d\n", V4[0], V4[1], V4[2], V4[3]);
	} // for i
	// ������ ��������� ����� � �����

	fclose(file);

	printf("\n");
	printf("CREATE SURFACE HYBRID MESH 3D INTERIOR TOPOLOGY AND FUNCS TECPLOT VISUAL FILE \"%s\"\n", fname);

	return 0;
}

int IOHM2D_PlotHybQuadTri2DMeshFunc(char *fname, 
									int NP, double *CRD,
									int NE, int *EX, int *EA,									
									int nF, double *func,
									char *varNames)
{
	int NE4 = 0;

	int i, j;
	FILE *file = NULL;

	for(i=0; i<NE; i++) if(EX[i+1] - EX[i] == 4) NE4 += 1;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri2DMeshFunc> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	// ������ ������������ ������ ����� �����
	fprintf(file, "VARIABLES= \"X\",\"Y\"");
	if(varNames == NULL) for(j=0; j<nF; j++) fprintf(file, ",\"VAR%d\"", j);
	else fprintf(file, "%s", varNames);
	fprintf(file, "\n");
	// ������ ������������ ������ ����� �����

	// ������ ��������� ����� � �����
	fprintf(file, "ZONE\n");

	if(NE4 == 0) fprintf(file, "F=FEBLOCK,      ET=TRIANGLE, N=%d E=%d VARLOCATION=([3", NP, NE);
	        else fprintf(file, "F=FEBLOCK, ET=QUADRILATERAL, N=%d E=%d VARLOCATION=([3", NP, NE);
	for(j=1; j<nF; j++) fprintf(file, ",%d", 3+j);
	fprintf(file, "]=CELLCENTERED)\n");

	for(i=0; i<NP; i++) { fprintf(file, "%g ", CRD[i*2 + 0]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	for(i=0; i<NP; i++) { fprintf(file, "%g ", CRD[i*2 + 1]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	for(j=0; j<nF; j++)
	for(i=0; i<NE; i++) { fprintf(file, "%g ", func[i*nF + j]); if((i != 0) && (i%10 == 0) ) fprintf(file, "\n"); } if((i-1)%10 != 0) fprintf(file, "\n");
	
	if(NE4 == 0) for(i=0; i<NE; i++) fprintf(file, "%d %d %d\n", EA[i*3 + 0] + 1, EA[i*3 + 1] + 1, EA[i*3 + 2] + 1);
	if(NE4 != 0) for(i=0; i<NE; i++)
	{
		int TYPE = EX[i+1] - EX[i];
		int *PTR = EA + EX[i];
		int V4[4];

		for(j=0; j<4; j++)
		{
			if(j < TYPE) V4[j] = PTR[j] + 1;
			        else V4[j] = V4[j-1];
		} // for j

		fprintf(file, "%d %d %d %d\n", V4[0], V4[1], V4[2], V4[3]);
	} // for i
	// ������ ��������� ����� � �����

	fclose(file);

	printf("\n");
	printf("CREATE SURFACE HYBRID MESH 2D INTERIOR TOPOLOGY AND FUNCS TECPLOT VISUAL FILE \"%s\"\n", fname);

	return 0;
}

int IOHM2D_PlotHybQuadTri3DMeshFuncVertex(char *fname, 
									      int NP, double *CRD,
									      int NE, int *EX, int *EA,									
									      int nF, double *func,
									      char *varNames)
{
	int NE4 = 0;

	int i, j;
	FILE *file = NULL;

	for(i=0; i<NE; i++) if(EX[i+1] - EX[i] == 4) NE4 += 1;

	file = fopen(fname, "w");
	if(file == NULL) { fprintf(stderr, "<IOHM2D_PlotHybQuadTri3DMeshFuncVertex> ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

	fprintf(file, "VARIABLES= \"X\",\"Y\",\"Z\"");
	if(varNames == NULL) for(j=0; j<nF; j++) fprintf(file, ",\"VAR%d\"", j);
	else fprintf(file, "%s", varNames);
	fprintf(file, "\n");
	
	fprintf(file, "ZONE\n");

	if(NE4 == 0) fprintf(file, "F=FEPOINT,      ET=TRIANGLE, N=%d E=%d\n", NP, NE);
	        else fprintf(file, "F=FEPOINT, ET=QUADRILATERAL, N=%d E=%d\n", NP, NE);

	for(i=0; i<NP; i++) 
	{
		fprintf(file, "%g %g %g", CRD[i*3 + 0], CRD[i*3 + 1], CRD[i*3 + 2]);
		for(j=0; j<nF; j++) fprintf(file, " %g", func[i*nF + j]);
		fprintf(file, "\n");
	} // for i
	
	if(NE4 == 0) for(i=0; i<NE; i++) fprintf(file, "%d %d %d\n", EA[i*3 + 0] + 1, EA[i*3 + 1] + 1, EA[i*3 + 2] + 1);
	if(NE4 != 0) for(i=0; i<NE; i++)
	{
		int TYPE = EX[i+1] - EX[i];
		int *PTR = EA + EX[i];
		int V4[4];

		for(j=0; j<4; j++)
		{
			if(j < TYPE) V4[j] = PTR[j] + 1;
			        else V4[j] = V4[j-1];
		} // for j

		fprintf(file, "%d %d %d %d\n", V4[0], V4[1], V4[2], V4[3]);
	} // for i
	
	fclose(file);

	printf("\n");
	printf("CREATE SURFACE HYBRID MESH 3D INTERIOR TOPOLOGY AND VERTEX FUNCS TECPLOT VISUAL FILE \"%s\"\n", fname);

	return 0;
}

int IOHM2D_PlotHybQuadTri2DMeshPart(int nP, double *c, int nE, int *x, int *a, int *part, char *fname)
{
	FILE *file;
	int *id;

	int partMIN, partMAX;
	int i, j, k;

	partMIN = partMAX = part[0];
	for(i=1; i<nE; i++)
	{
		if(partMIN > part[i]) partMIN = part[i];
		if(partMAX < part[i]) partMAX = part[i];
	} // for i

	id = (int *)malloc(nP * sizeof(int)); if(id == NULL) exit(0);
	file = fopen(fname, "w"); if(file == NULL) exit(0);

	fprintf(file, "VARIABLES= \"X\",\"Y\"\n");

	for(i=partMIN; i<=partMAX; i++)
	{
		int nPL = 0;
		int nEL = 0;

		for(j=0; j<nP; j++) id[j] = -1;
		for(j=0; j<nE; j++) if(part[j] == i)
		{
			nEL += 1;
			for(k=x[j]; k<x[j+1]; k++) id[a[k]] = 0;
		} // if
		for(j=0; j<nP; j++) if(id[j] == 0) id[j] = nPL + 1, nPL += 1;

		if( (nPL != 0) && (nEL != 0) )
		{
			fprintf(file, "ZONE T=\"SUBDOMEN %d\"\n", i);
			fprintf(file, "F=FEPOINT, ET=QUADRILATERAL, N=%d E=%d\n", nPL, nEL);

			for(j=0; j<nP; j++) if(id[j] > 0) fprintf(file, "%g %g\n", c[j*2 + 0], c[j*2 + 1]);
			for(j=0; j<nE; j++) if(part[j] == i)
			{
				int *PTR = a + x[j];
				int TYPE = x[j+1] - x[j];

				if(TYPE == 3) fprintf(file, "%d %d %d %d\n", id[PTR[0]], id[PTR[1]], id[PTR[2]], id[PTR[2]]);
				if(TYPE == 4) fprintf(file, "%d %d %d %d\n", id[PTR[0]], id[PTR[1]], id[PTR[2]], id[PTR[3]]);
			} // for j
		} // if
	} // for i

	fclose(file);
	free(id);

	return 0;
}
