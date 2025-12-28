#define __EXT_YES__ 1
#define __EXT_NO__ 0

#define _USE_REVERSE_CM_ __EXT_YES__
#define _USE_NEGATIVE_ELEMENTS_ORDER_ __EXT_YES__
#define _USE_NEGATIVE_NODES_ORDER_ __EXT_YES__

#define _MESH_FOLDER_NAME_ "../../../MESH/LE_fine/"
// #define _MESH_FOLDER_NAME_ "../../../MESH/LE_medium/"
// #define _MESH_FOLDER_NAME_ "../../../MESH/WIND-FARMS-area-1/"


#include <stdio.h>
#include <stdlib.h>

#include "ioHM2D.h"
#include "hybmeshdual2D.h"
#include "csrProcessCuthillMcKee.h"
#include "csrProcessDichotomy.h"

int main(void) {

    t_MeshHybQuadTri2D iMesh;
    int *iNodeType = NULL;
    int numLevs = -1;

    // ВЫВОД НА ЭКРАН ЗАГОЛОВКА
    {
        printf("\n");

        printf("FESOM-C mesh element and node reordering application\n");
        printf("Copyright (c) 2025-2026 Sergey Sukov, FZJ\n");
        printf("Unauthorized use or distribution is prohibited.\n");
        printf("\n");

        printf("                     MESH FOLDER: %s\n", _MESH_FOLDER_NAME_);
        if(_USE_REVERSE_CM_ == __EXT_YES__)
        printf("BASIC GRAPH REORDERING ALGORITHM: REVERSE CUTHILL-MCKEE\n");
        else
        printf("BASIC GRAPH REORDERING ALGORITHM: CUTHILL-MCKEE\n");

        if( (_USE_NEGATIVE_ELEMENTS_ORDER_ == __EXT_YES__) && (_USE_NEGATIVE_NODES_ORDER_ == __EXT_YES__) ) {
            printf("********** OPTIONAL EXPERIMENTAL FEATURES **********\n");
            if(_USE_NEGATIVE_ELEMENTS_ORDER_ == __EXT_YES__) printf("+++ RANDONIZED ELEMENT ORDERING\n");
            if(_USE_NEGATIVE_NODES_ORDER_    == __EXT_YES__) printf("+++ RANDONIZED    NODE ORDERING\n");
            printf("****************************************************\n");
        }        

        printf("\n"); fflush(stdout);
    }
    // ВЫВОД НА ЭКРАН ЗАГОЛОВКА

    // СЧИТЫВАНИЕ КООРДИНАТ УЗЛОВ
    {
        char fname[512]; sprintf(fname, "%snod2d.out", _MESH_FOLDER_NAME_);
        printf("Nodal file name: %s\n", fname);

        char line[1024], *oldstr, *newstr;
        int id;

	    FILE *file = fopen(fname, "r");
        if(file == NULL) { fprintf(stderr, "ERROR: CAN'T OPEN FILE %s\n", fname); exit(0); }

        fgets(line, 500, file); oldstr = line; newstr = NULL;
        iMesh.NP  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
        printf("Number of mesh nodes: %d\n", iMesh.NP);

        iNodeType = new int    [iMesh.NP    ];
        iMesh.CRD = new double [iMesh.NP * 3];

        for(int i=0; i<iMesh.NP; i++) {
		    fgets(line, 500, file); oldstr = line; newstr = NULL;
            id  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
            id = id - 1;
		    iMesh.CRD[id*3 + 0] = (double)strtod(oldstr, &newstr); oldstr = newstr;
		    iMesh.CRD[id*3 + 1] = (double)strtod(oldstr, &newstr); oldstr = newstr;
            iNodeType[id]  = (int)strtol(oldstr, &newstr, 10);
	    } // for i

        fclose(file);

        printf("Read X & Y coordinates & node types\n");
        printf("\n");

        sprintf(fname, "%saux3d.out", _MESH_FOLDER_NAME_);
        printf("Depth file name: %s\n", fname);

        file = fopen(fname, "r");
        if(file == NULL) { fprintf(stderr, "ERROR: CAN'T OPEN FILE %s\n", fname); exit(0); }

        fgets(line, 500, file); oldstr = line; newstr = NULL;
        numLevs  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
        printf("Number of levels: %d\n", numLevs);

        for(int i=0; i<numLevs; i++) fgets(line, 500, file);
        printf("Skip levels ids\n");

        for(int i=0; i<iMesh.NP; i++) {
		    fgets(line, 500, file); oldstr = line; newstr = NULL;
            iMesh.CRD[i*3 + 2] = (double)strtod(oldstr, &newstr);
	    } // for i

        fclose(file);

        printf("Read Z coordinates of nodes\n");
        printf("\n");
    }
    // СЧИТЫВАНИЕ КООРДИНАТ УЗЛОВ

    // СЧИТЫВАНИЕ ТОПОЛОГИИ СЕТОЧНЫХ ЭЛЕМЕНТОВ
    {
        char fname[512]; sprintf(fname, "%selem2d.out", _MESH_FOLDER_NAME_);
        printf("Elements file name: %s\n", fname);

        char line[1024], *oldstr, *newstr;

        FILE *file = fopen(fname, "r");
        if(file == NULL) { fprintf(stderr, "ERROR: CAN'T OPEN FILE %s\n", fname); exit(0); }

        fgets(line, 500, file); oldstr = line; newstr = NULL;
        iMesh.NE  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
        printf("Number of mesh elements: %d\n", iMesh.NE);

        iMesh.NE3 = iMesh.NE4 = iMesh.NBE = 0;
        iMesh.XE = new int [iMesh.NE + 1]; for(int i=0; i<=iMesh.NE  ; i++) iMesh.XE[i] =  0;
        iMesh.AE = new int [iMesh.NE * 4]; for(int i=0; i< iMesh.NE*4; i++) iMesh.AE[i] = -1;

        for(int i=0; i<iMesh.NE; i++) {
            int eIds[4] = {-1, -1, -1, -1};
		    fgets(line, 527, file); oldstr = line; newstr = NULL;

            for(int j=0; j<4; j++) {
                eIds[j]  = (int)strtol(oldstr, &newstr, 10); oldstr = newstr;
                eIds[j] -= 1;
            } // for j 
            
            int nVtx = 4; if(eIds[0] == eIds[3]) nVtx = 3;
            for(int j=0; j<nVtx; j++) iMesh.AE[iMesh.XE[i] + j] = eIds[j];
            iMesh.XE[i + 1] = iMesh.XE[i] + nVtx;

            if(nVtx == 4) iMesh.NE4 += 1;
            if(nVtx == 3) iMesh.NE3 += 1;
	    } // for i

        fclose(file);

        printf("Number of triangles: %8d\n", iMesh.NE3);
        printf("Number of     quads: %8d\n", iMesh.NE4);
        if(iMesh.NE3 + iMesh.NE4 != iMesh.NE) { fprintf(stderr, "ERROR: iMesh.NE3 + iMesh.NE4 != iMesh.NE\n"); exit(0); }

        printf("Read topology of elements\n");        
    }
    // СЧИТЫВАНИЕ ТОПОЛОГИИ СЕТОЧНЫХ ЭЛЕМЕНТОВ

    // ВИЗУАЛИЗАЦИЯ ТОПОЛОГИИ СЕТКИ В ФОРМАТЕ TecPlot
    IOHM2D_PlotHybQuadTri3DMeshInterior("../output/mesh.dat", iMesh.NP, iMesh.CRD, iMesh.NE, iMesh.XE, iMesh.AE);
    printf("\n");
    // ВИЗУАЛИЗАЦИЯ ТОПОЛОГИИ СЕТКИ В ФОРМАТЕ TecPlot

    // ГЕНЕРАЦИЯ ДУАЛЬНОГО ГРАФА СЕТКИ
    int *csrX, *csrA;
    GenDualCSRForHybMesh2D(iMesh.NE, iMesh.XE, iMesh.AE, &csrX, &csrA);
    printf("\n");
    // ГЕНЕРАЦИЯ ДУАЛЬНОГО ГРАФА СЕТКИ

    // ОПРЕДЕЛЕНИЕ ИНДЕКСА ПСВЕДОПЕРИФЕРИЙНОЙ ВЕРШИНЫ ДУАЛЬНОГО ГРАФА
    int idFirst = PseudoPeripheralCsrGraphVertex(iMesh.NE, csrX, csrA, 1);
    printf("\n");
    // ОПРЕДЕЛЕНИЕ ИНДЕКСА ПСВЕДОПЕРИФЕРИЙНОЙ ВЕРШИНЫ ДУАЛЬНОГО ГРАФА

    // ВВОД ЛОКАЛЬНОЙ ИНДЕКСАЦИИ В СООТВЕТСТВИИ С АЛГОРИТМОМ КАТХИЛЛА-МАККИ
    int *eLocalToGlobal = new int [iMesh.NE]; for(int i=0; i<iMesh.NE; i++) eLocalToGlobal[i] = -1;
    int *eGlobalToLocal = new int [iMesh.NE]; for(int i=0; i<iMesh.NE; i++) eGlobalToLocal[i] = -1;
    csrGraphRenameIndexCuthillMcKee(iMesh.NE, idFirst, csrX, csrA, eLocalToGlobal);    
    // ВВОД ЛОКАЛЬНОЙ ИНДЕКСАЦИИ В СООТВЕТСТВИИ С АЛГОРИТМОМ КАТХИЛЛА-МАККИ

    // ОБРАТНАЯ ИНДЕКСАЦИЯ КАТХИЛЛА-МАККИ
    if(_USE_REVERSE_CM_) {
        for(int i=0; i<iMesh.NE; i++) eGlobalToLocal[i] = eLocalToGlobal[iMesh.NE - 1 - i];
        for(int i=0; i<iMesh.NE; i++) eLocalToGlobal[i] = eGlobalToLocal[i];
        for(int i=0; i<iMesh.NE; i++) eGlobalToLocal[i] = -1;
    }
    // ОБРАТНАЯ ИНДЕКСАЦИЯ КАТХИЛЛА-МАККИ

    // ТРАНСФОРМАЦИЯ ИНДЕКСАЦИИ ЭЛЕМЕНТОВ В НЕГАТИВНУЮ
    if(_USE_NEGATIVE_ELEMENTS_ORDER_) {
        csrDichotomyNumericalSequence(iMesh.NE, eGlobalToLocal);
        for(int i=0; i<iMesh.NE; i++) eGlobalToLocal[i] = eLocalToGlobal[eGlobalToLocal[i]];
        for(int i=0; i<iMesh.NE; i++) eLocalToGlobal[i] = eGlobalToLocal[i];
        for(int i=0; i<iMesh.NE; i++) eGlobalToLocal[i] = -1;
    }
    // ТРАНСФОРМАЦИЯ ИНДЕКСАЦИИ ЭЛЕМЕНТОВ В НЕГАТИВНУЮ

    // ИНИЦИАЛИЗАЦИЯ СООТВЕТСТВИЯ НОВЫХ ИНДЕКСОВ ЭЛЕМЕНТОВ ИХ ИСХОДНЫМ ИНДЕКСАМ
    for(int i=0; i<iMesh.NE; i++) eGlobalToLocal[eLocalToGlobal[i]] = i;
    // ИНИЦИАЛИЗАЦИЯ СООТВЕТСТВИЯ НОВЫХ ИНДЕКСОВ ЭЛЕМЕНТОВ ИХ ИСХОДНЫМ ИНДЕКСАМ

    printf("MESH ELEMENTS HAVE BEEN REORDERED\n");

    // ИНИЦИАЛИЗАЦИЯ НОВОЙ ИНДЕКСАЦИИ УЗЛОВ СЕТКИ
    int *vLocalToGlobal = new int [iMesh.NP]; for(int i=0; i<iMesh.NP; i++) vLocalToGlobal[i] = -1;
    int *vGlobalToLocal = new int [iMesh.NP]; for(int i=0; i<iMesh.NP; i++) vGlobalToLocal[i] = -1;
    int nvSet = 0;

    for(int i=0; i<iMesh.NE; i++) {

        int eId = eLocalToGlobal[i];
        int *ePtr = iMesh.AE + iMesh.XE[eId];
        int N = iMesh.XE[eId + 1] - iMesh.XE[eId];

        for(int j=0; j<N; j++) {

            int vId = ePtr[j];

            if(vGlobalToLocal[vId] == -1) {

                if(nvSet == iMesh.NP) { fprintf(stderr, "ERROR: nvSet > iMesh.NP\n"); exit(0); }

                vGlobalToLocal[vId] = nvSet;
                vLocalToGlobal[nvSet] = vId;

                nvSet += 1;
            } // if

        } // for j

    } // for i

    if(nvSet != iMesh.NP) { fprintf(stderr, "ERROR: nvSet != iMesh.NP\n"); exit(0); }
    // ИНИЦИАЛИЗАЦИЯ НОВОЙ ИНДЕКСАЦИИ УЗЛОВ СЕТКИ

    // ТРАНСФОРМАЦИЯ ИНДЕКСАЦИИ УЗЛОВ СЕТКИ В НЕГАТИВНУЮ
    if(_USE_NEGATIVE_NODES_ORDER_) {
        csrDichotomyNumericalSequence(iMesh.NP, vGlobalToLocal);
        for(int i=0; i<iMesh.NP; i++) vGlobalToLocal[i] = vLocalToGlobal[vGlobalToLocal[i]];
        for(int i=0; i<iMesh.NP; i++) vLocalToGlobal[i] = vGlobalToLocal[i];
        for(int i=0; i<iMesh.NP; i++) vGlobalToLocal[i] = -1;
        for(int i=0; i<iMesh.NP; i++) vGlobalToLocal[vLocalToGlobal[i]] = i;
    }
    // ТРАНСФОРМАЦИЯ ИНДЕКСАЦИИ УЗЛОВ СЕТКИ В НЕГАТИВНУЮ

    printf("MESH NODES HAVE BEEN REORDERED\n");
    printf("\n");

    // ЗАПИСЬ КООРДИНАТ УЗЛОВ
    {
        char fname[512]; sprintf(fname, "../output/nod2d.out");
        printf("Nodal file name: %s\n", fname);        

	    FILE *file = fopen(fname, "w");
        if(file == NULL) { fprintf(stderr, "ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

        fprintf(file, "%12d\n", iMesh.NP);
        printf("Number of mesh nodes: %d\n", iMesh.NP);

        for(int i=0; i<iMesh.NP; i++) {
            int id = vLocalToGlobal[i];		    
            fprintf(file, "%12d %20.12lf %25.12lf %16d\n", i+1, iMesh.CRD[id*3 + 0], iMesh.CRD[id*3 + 1], iNodeType[id]);
	    } // for i

        fclose(file);

        printf("Write X & Y coordinates & node types\n");
        printf("\n");

        sprintf(fname, "../output/aux3d.out");
        printf("Depth file name: %s\n", fname);

        file = fopen(fname, "w");
        if(file == NULL) { fprintf(stderr, "ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

        fprintf(file, "%12d\n", numLevs);
        printf("Number of levels: %d\n", numLevs);

        for(int i=0; i<numLevs; i++) fprintf(file, "%12d\n", i);
        printf("Write levels ids\n");

        for(int i=0; i<iMesh.NP; i++) {
            int id = vLocalToGlobal[i];
            fprintf(file, "%12.6lf\n", iMesh.CRD[id*3 + 2]);
        } // for i
        
        fclose(file);

        printf("Write Z coordinates of nodes\n");
        printf("\n");
    }
    // ЗАПИСЬ КООРДИНАТ УЗЛОВ

    // ЗАПИСЬ ТОПОЛОГИИ СЕТОЧНЫХ ЭЛЕМЕНТОВ
    {
        char fname[512]; sprintf(fname, "../output/elem2d.out");
        printf("Elements file name: %s\n", fname);

        FILE *file = fopen(fname, "w");
        if(file == NULL) { fprintf(stderr, "ERROR: CAN'T CREATE FILE %s\n", fname); exit(0); }

        fprintf(file, "%12d\n", iMesh.NE);
        printf("Number of mesh elements: %d\n", iMesh.NE);

        for(int i=0; i<iMesh.NE; i++) {
            int vIds[4] = {-1, -1, -1, -1};
            int eId = eLocalToGlobal[i];
            int *ePtr = iMesh.AE + iMesh.XE[eId];
            int N = iMesh.XE[eId + 1] - iMesh.XE[eId];

            for(int j=0; j<N; j++) {
                int vId = ePtr[j];
                vIds[j] = vGlobalToLocal[vId] + 1;
            } // for j
            if(N == 3) vIds[3] = vIds[0];

            fprintf(file, "%12d %11d %11d %11d\n", vIds[0], vIds[1], vIds[2], vIds[3]);
        } // for i

        fclose(file);
        
        printf("Write topology of elements\n");
        printf("\n");
    }
    // ЗАПИСЬ ТОПОЛОГИИ СЕТОЧНЫХ ЭЛЕМЕНТОВ

    return 0;
}
