/*
 * Software module implementing graph reordering subroutines
 *
 * Copyright (c) 2025–2026 Sergey Sukov
 * All rights reserved.
 *
 * This source code is proprietary and confidential.
 * Unauthorized copying, distribution, modification, or use
 * of this file, via any medium, is strictly prohibited.
 */

#include <stdio.h>
#include <stdlib.h>

#include "csrProcessCuthillMcKee.h"

int csrGraphRenameIndexCuthillMcKee(int n, int idFIRST, int *x, int *a, int *index)
{
	int *useFLAG = new int [n];
	int nTUPLE, nPROCESS;

	int i;

	// начальная инициализация
	if(useFLAG == NULL) { fprintf(stderr, "csrGraphRenameIndexCuthillMcKee: ERROR #1 Can't allocate memory\n"); fflush(stderr); exit(0); }
	for(i=0; i<n; i++)
	{
		useFLAG[i] = x[i + 1] - x[i];
		if(useFLAG[i] <= 0) { fprintf(stderr, "csrGraphRenameIndexCuthillMcKee: ERROR #2 nLINK[%d] = %d\n", i, useFLAG[i]); fflush(stderr); exit(0); }
	}

	// добавление стартового элемента в кортеж
	index[0] = idFIRST; useFLAG[idFIRST] = 0; nTUPLE = 1;

	// основной цикл
	nPROCESS = 0;
	while(nTUPLE != n)
	{
		int id = index[nPROCESS]; // оригинальный индекс обрабатываемого элемента кортежа
		int *aPTR = a + x[id]; // указатель на начало массива с индексами элементов-связей для обрабатываемого элемента
		int nLINK = x[id + 1] - x[id]; // общее число элементов-связей

		int *sortPTR = index + nTUPLE; // указатель на начало массива для списка индексов добавляемых в кортеж элементов
		int nSORT = 0; // число добавляемых в кортеж элементов

		// составление списка добавляемых в кортеж элементов
		for(i=0; i<nLINK; i++) if(useFLAG[aPTR[i]] > 0)
		{
			sortPTR[nSORT] = aPTR[i];
			nSORT += 1;
		} // for i

		// пузырьковая сортировка списка добавляемых в кортеж элементов по их весам
		for(i=0; i<nSORT-1; i++)
		{
			int j;

			for(j=0; j<nSORT-1-i; j++)
			{
				if(   (useFLAG[sortPTR[j]] >  useFLAG[sortPTR[j + 1]])
					||
					( (useFLAG[sortPTR[j]] == useFLAG[sortPTR[j + 1]]) && (sortPTR[j] > sortPTR[j + 1]) )
				  )
				{ int tmp; tmp = sortPTR[j + 1]; sortPTR[j + 1] = sortPTR[j]; sortPTR[j] = tmp; } // if
			} // for j
		} // for i

		// обновление кортежа
		for(i=0; i<nSORT; i++) useFLAG[sortPTR[i]] = 0;
		nTUPLE += nSORT;

		// переход к новому обрабатываемому элементу
		nPROCESS += 1;

		// проверка корректности текущего статуса очереди
		if( (nPROCESS >= nTUPLE) && (nTUPLE != n) ) { fprintf(stderr, "csrGraphRenameIndexCuthillMcKee: ERROR #3 Not connected graph\n"); fflush(stderr); exit(0); }
	} // while

	delete [] useFLAG;	

	return 0;
}

int PseudoPeripheralCsrGraphVertex(int n, int *x, int *a, int printfFlag)
{
	int *level = new int [n];
	int *index = new int [n];

	int nList;
	int pVertexIndex, pVertexLevel;

	int iVtx, iLink;

	if(level == NULL) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #1\n"); fflush(stderr); exit(0); }
	if(index == NULL) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #2\n"); fflush(stderr); exit(0); }

	if(printfFlag) printf("START  --- PseudoPeripheralCsrGraphVertex ---\n");

	for(iVtx=0; iVtx<n; iVtx++)
	{
		level[iVtx] = -1;
		index[iVtx] = -1;
	} // for iVtx

	pVertexIndex = 0;
	level[pVertexIndex] = 0; index[0] = pVertexIndex; nList = 1;
	for(iVtx=0; iVtx<n; iVtx++)
	{
		if(index[iVtx]        == -1) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #3\n"); fflush(stderr); exit(0); }
		if(level[index[iVtx]] == -1) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #4\n"); fflush(stderr); exit(0); }

		for(iLink=x[index[iVtx]]; iLink<x[index[iVtx] + 1]; iLink++)
		{
			if(level[a[iLink]] == -1)
			{
				level[a[iLink]] = level[index[iVtx]] + 1;
				index[nList] = a[iLink];
				nList += 1;
			} // if
		} // for iLink
	} // for iVtx
	if(nList != n) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #5\n"); fflush(stderr); exit(0); }
	pVertexLevel = level[index[nList - 1]];
	if(printfFlag) printf("Start from vertex %d (vertex level %d)\n", pVertexIndex, pVertexLevel);

	while(1)
	{
		int cVertexIndex, cVertexLevel;

		// ПОИСК НОВОЙ ПОТЕНЦИАЛЬНОЙ ПСЕВДОПЕРИФЕРИЙНОЙ ВЕРШИНЫ
		{
			int idMin, nLinkMin;

			int i;

			for(i=n-1; i>0; i--)
			{
				if(level[index[i-1]] == pVertexLevel - 1)
					break;
			} // for i

			idMin = index[i]; nLinkMin = x[index[i] + 1] - x[index[i]];
			for(i=i+1; i<n; i++)
			{
				int nLinkCur = x[index[i] + 1] - x[index[i]];
				if(nLinkMin > nLinkCur)
				{
					idMin = index[i];
					nLinkMin = nLinkCur;
				} // if
			} // for i

			cVertexIndex = idMin;
			if(printfFlag) printf("Vertex %d Link number %d\n", cVertexIndex, nLinkMin);
		}
		// ПОИСК НОВОЙ ПОТЕНЦИАЛЬНОЙ ПСЕВДОПЕРИФЕРИЙНОЙ ВЕРШИНЫ

		for(iVtx=0; iVtx<n; iVtx++)
		{
			level[iVtx] = -1; index[iVtx] = -1;
		} // for iVtx

		level[cVertexIndex] = 0; index[0] = cVertexIndex; nList = 1;
		for(iVtx=0; iVtx<n; iVtx++)
		{
			if(index[iVtx]        == -1) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #6\n"); fflush(stderr); exit(0); }
			if(level[index[iVtx]] == -1) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #7\n"); fflush(stderr); exit(0); }

			for(iLink=x[index[iVtx]]; iLink<x[index[iVtx] + 1]; iLink++)
			{
				if(level[a[iLink]] == -1)
				{
					level[a[iLink]] = level[index[iVtx]] + 1;
					index[nList] = a[iLink];
					nList += 1;
				} // if
			} // for iLink
		} // for iVtx
		if(nList != n) { fprintf(stderr, "PseudoPeripheralCsrGraphVertex: ERROR #8\n"); fflush(stderr); exit(0); }
		cVertexLevel = level[index[nList - 1]];
		if(printfFlag) printf("Current vertex %d (vertex level %d)\n", cVertexIndex, cVertexLevel);
		
		if(cVertexLevel > pVertexLevel)
		{
			pVertexIndex = cVertexIndex; pVertexLevel = cVertexLevel;
		}
		else
		{
			pVertexIndex = cVertexIndex; pVertexLevel = cVertexLevel;
			break;
		}
	} // while

	delete [] level; delete [] index;

	if(printfFlag) printf("FINISH --- PseudoPeripheralCsrGraphVertex ---\n");

	return pVertexIndex;
}

int csrGraphRenameIndexKing(int n, int *x, int *a, int *index)
{
	int *nLink = new int [n];
	int *eType = new int [n];

	int firstIndex;
	int nAB;
	
	int i, j, k;

	// ПОИСК СТАРТОВОЙ ЯЧЕЙКИ С МИНИМАЛЬНЫМ ЧИСЛОМ СВЯЗЕЙ
	if(nLink == NULL) { fprintf(stderr, "csrGraphRenameIndexKing: ERROR --> #1# Can't allocate memory\n"); exit(0); }
	for(i=0; i<n; i++) nLink[i] = x[i + 1] - x[i]; 
		
	firstIndex = 0; for(i=1; i<n; i++) if(nLink[i] < nLink[firstIndex]) firstIndex = i;
	printf("first cell is %9d nlink = %2d\n", firstIndex, nLink[firstIndex]);

	// ТИПЫ ЯЧЕЕК: 2 - проиндексированная, 1 - связь с проиндексированными ячеками, 0 - оставшиеся ячейки
	// по итогам процедуры все ячейки должны получить второй тип, а число их активных связей должно стать 0
	if(eType == NULL) { fprintf(stderr, "csrGraphRenameIndexKing: ERROR --> #2# Can't allocate memory\n"); exit(0); }
	for(i=0; i<n; i++) eType[i] = 0;

	// ГЕНЕРАЦИЯ НАЧАЛА СПИСКА
	for(i=0; i<n; i++) index[i] = -1;
	index[0] = firstIndex; eType[firstIndex] = 2;

	for(i=0; i<nLink[firstIndex]; i++) 
	{
		index[1 + i] = a[x[firstIndex] + i];
		eType[a[x[firstIndex] + i]]  = 1;
		nLink[a[x[firstIndex] + i]] -= 1;
		// printf("ADD %d\n", a[x[firstIndex] + i]);
	} // for i
	nAB = 1 + nLink[firstIndex];

	for(i=1; i<nAB; i++)
	{
		int id = index[i];
		for(j=x[id]; j<x[id + 1]; j++) nLink[a[j]] -= 1;		
	} // for i
	nLink[firstIndex] = 0;

	// ОСНОВНОЙ ЦИКЛ
	for(i=1; i<n-1; i++)
	{
		int nAdd = 0;

		if(index[i] == -1) { fprintf(stderr, "csrGraphRenameIndexKing: ERROR --> #3# No active cell\n"); exit(0); }

		// ВЫБОР СЛЕДУЮЩЕЙ ИНДЕКСИРУЕМОЙ ЯЧЕЙКИ
		{
			int posMin = i;
			for(j=i+1; j<nAB; j++) if(nLink[posMin] > nLink[index[i]]) posMin = i;
			if(i%5000 == 0) printf("set %d of %d\n", i, n); // printf(" next cell is %9d nlink = %2d\n", index[posMin], nLink[index[posMin]]);
			if(posMin != i) { j = index[i]; index[i] = index[posMin]; index[posMin] = j; }
		}
		// ВЫБОР СЛЕДУЮЩЕЙ ИНДЕКСИРУЕМОЙ ЯЧЕЙКИ

		// ДОБАВЛЕНИЕ СВЯЗЕЙ ТИПА C В КОНЕЦ СПИСКА
		for(j=x[index[i]]; j<x[index[i] + 1]; j++)
			if(eType[a[j]] == 0) 
			{ 
				index[nAB + nAdd] = a[j]; 
				eType[a[j]] = 1; 				
				nAdd += 1; 
			} // if
		
		// КОРРЕКТИРОВКА ЧИСЛА СВЯЗЕЙ ЯЧЕЕК
		for(j=nAB; j<nAB + nAdd; j++)
		{
			// printf("check %d\n", index[j]);
			for(k=x[index[j]]; k<x[index[j] + 1]; k++)
			{
				// printf("test for %d %d\n", a[k], eType[a[k]]);
				if(eType[a[k]] == 2) { fprintf(stderr, "csrGraphRenameIndexKing: ERROR --> #4# Wrong link type\n"); exit(0); }
				nLink[a[k]] -= 1;
				if(nLink[a[k]] < 0) { fprintf(stderr, "csrGraphRenameIndexKing: ERROR --> #5# Negative link number\n"); exit(0); }				
			} // for k
		} // for j

		// ПРОВЕРКА ЧИСЛА СВЯЗЕЙ АКТИВНОГО ЭЛЕМЕНТА
		if(nLink[index[i]] != 0) 
		{ 
			fprintf(stderr, "csrGraphRenameIndexKing: ERROR --> #6# Wrong central cell link number %d\n", nLink[index[i]]); 
			for(k=x[index[i]]; k<x[index[i] + 1]; k++)
				printf("%9d t %2d l %2d\n", a[k], eType[a[k]], nLink[a[k]]);
			exit(0); 
		}
		eType[index[i]] = 2;
		nAB += nAdd;
		// exit(0);

		if(0)
		{
			for(j=0; j<nAB; j++)
			{
				int nL = 0;

				for(k=x[index[j]]; k<x[index[j] + 1]; k++)
					if(eType[a[k]] == 0)
					{
						nL += 1;
					} // if

					if(nLink[index[j]] != nL) { fprintf(stderr, "\nERR %d > is %d and must %d\n", index[j], nLink[index[j]], nL); exit(0); }
			} // for j
		}

		// printf("\n>>> %d\n\n", nLink[111385]);
	} // for i	

	delete [] nLink; delete [] eType;

	printf(">>>>>\n");

	return 0;
}


#define W1 0
#define W2 1
#define _DEBUG_PRINTF_ 1
int csrGraphRenameIndexSloan(int N, int idFIRST, int *x, int *a, int *index)
{
	int l = 0; // число вершин, которым были выданы новые индексы
	int *d = new int [N]; // степень вершины = число соседей
	int *q = new int [N]; int n; // очередь с приоритетами и ее длина
	int *p = new int [N]; // приоритеты вершин графа
	int *delta = new int [N]; // расстояние от вершин графа до вершины idFIRST (начало псевдо-диаметра) 
	
	char *status = new char [N]; // статусы вершин графа
	// 'l' - labled or postactive
	// 'a' - active
	// 'p' - preactive
	// 'i' - inactive

	int i, j, k;

	if( (d == NULL) || (q == NULL) || (p == NULL) || (delta == NULL) || (status == NULL) )
	{ fprintf(stderr, "csrGraphRenameIndexSloan: ERROR #1\n"); exit(0); }

	// ИНИЦИАЛИЗАЦИЯ РАССТОЯНИЙ ДО НАЧАЛА ПСЕВДО-ДИАМЕТРА
	for(i=0; i<N; i++) delta[i] = index[i] = -1;
	delta[idFIRST] = 0; index[0] = idFIRST; k = 1;
	
	for(i=0; i<N; i++)
	{
		if(index[i]        == -1) { fprintf(stderr, "csrGraphRenameIndexSloan: ERROR #2\n"); exit(0); }
		if(delta[index[i]] == -1) { fprintf(stderr, "csrGraphRenameIndexSloan: ERROR #3\n"); exit(0); }

		for(j=x[index[i]]; j<x[index[i] + 1]; j++)
		{
			if(delta[a[j]] == -1)
			{
				delta[a[j]] = delta[index[i]] + 1;
				index[k] = a[j];
				k += 1;
			} // if
		} // for j
	} // for i
	if(k != N) { fprintf(stderr, "csrGraphRenameIndexSloan: ERROR #4\n"); exit(0); }
	if(_DEBUG_PRINTF_) printf("START VERTEX = %d MAXIMUM DISTANCE = %d\n", idFIRST, delta[index[N-1]]);
	// ИНИЦИАЛИЗАЦИЯ РАССТОЯНИЙ ДО НАЧАЛА ПСЕВДО-ДИАМЕТРА

	for(i=0; i<N; i++) q[i] = -1; n = 0;
	
	for(i=0; i<N; i++) d[i] = x[i + 1] - x[i];	
	if(_DEBUG_PRINTF_)
	{
		j = k = d[0];
		for(i=1; i<N; i++)
		{
			if(j > d[i]) j = d[i];
			if(k < d[i]) k = d[i];
		} // for i
		printf("DEGREE = [%d; %d]\n", j, k);
	} // if

	for(i=0; i<N; i++) index[i] = q[i] = -1;

	for(i=0; i<N; i++) status[i] = 'i';

	for(i=0; i<N; i++) p[i] = W1 * delta[i] - W2 * (delta[i] + 1);
	if(_DEBUG_PRINTF_)
	{
		j = k = p[0];
		for(i=1; i<N; i++)
		{
			if(j > d[i]) j = d[i];
			if(k < d[i]) k = d[i];
		} // for i
		printf("PRIORITY = [%d; %d]\n", j, k);
	} // if

	q[0] = idFIRST; n = 1;
	status[idFIRST] = 'p';
	l = 0;
	while(n > 0)
	{
		int m = 0;
		int nodeToLabbled;
		
		// поиск в очереди вершины графа с максимальным приоритетом
		for(i=1; i<n; i++)
		{
			if(p[q[m]] < p[q[i]]) m = i;			
		} // for i
		nodeToLabbled = q[m];
		// if(_DEBUG_PRINTF_) printf("NODE %d PRIORITY %d\n", nodeToLabbled, p[nodeToLabbled]);
		// поиск в очереди вершины графа с максимальным приоритетом

		// удаление верщины графа nodeToLabbled из очереди
		if(m != (n - 1)) q[m] = q[n - 1];
		n -= 1;
		// удаление верщины графа nodeToLabbled из очереди

		// обработка вершины графа типа "preactive"
		if(status[nodeToLabbled] == 'p')
		{
			for(j=x[nodeToLabbled]; j<x[nodeToLabbled + 1]; j++)
			{
				p[a[j]] += W2;

				if(status[a[j]] == 'i')
				{
					q[n] = a[j]; n += 1;
					status[a[j]] = 'p';
				} // if status[a[j]] == 'i'
			} // for j
		} // if status[nodeToLabbled] == 'p'
		// обработка вершины графа типа "preactive"

		// выдача нового индекса вершине графа nodeToLabbled
		index[l] = nodeToLabbled; l += 1;
		status[nodeToLabbled] = 'l';
		if( (N > 500000) && (l > 0) && (l % 100000 == 0) && (_DEBUG_PRINTF_) )
			printf("%d nodes of %d indexed + Current queue %d nodes\n", l, N, n);
		// выдача нового индекса вершине графа nodeToLabbled

		// обновление приоритетов и очереди
		for(j=x[nodeToLabbled]; j<x[nodeToLabbled + 1]; j++)
			if(status[a[j]] == 'p')
			{
				status[a[j]] = 'a';
				p[a[j]] += W2;

				for(k=x[a[j]]; k<x[a[j]+1]; k++)
					if(status[a[k]] != 'l')
					{
						p[a[k]] += W2;
						if(status[a[k]] == 'i')
						{
							q[n] = a[k]; n += 1;
							status[a[k]] = 'p';
						} // if status[a[k]] == 'i'
					} // for k				
			} // for j --> if status[a[j]] == 'p'
		// обновление приоритетов и очереди		
	} // while

	if(_DEBUG_PRINTF_) printf("FINISH\n");
	if(l != N)
	{
		fprintf(stderr, "csrGraphRenameIndexSloan: ERROR #5 l = %d of %d\n", l, N); 
		k = 0; 
		for(i=0; i<N; i++) 
			if(status[i] != 'l') 
			{
				printf("ID = %d STATUS = %c DISTANCE = %d\n", i, status[i], delta[i]); 
				k += 1;
			}
		printf("TOTAL FREE NODES NUMBER = %d\n", k);		
		exit(0);
	}



	return 0;
}
#undef W1
#undef W2
#undef _DEBUG_PRINTF_
