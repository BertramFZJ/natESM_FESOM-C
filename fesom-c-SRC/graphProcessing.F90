MODULE graphProcessing

    IMPLICIT NONE

#ifdef USE_MPI
    include 'mpif.h'
#endif

    PRIVATE

    PUBLIC :: setDualGraphEdgeColor
    PUBLIC :: numFvFaceColor, fvFaceColorX, fvFaceColorA

    PUBLIC :: initFvFaceToCellMap
    PUBLIC :: numFvCell, fvFaceToCellX, fvFaceToCellA

    INTEGER :: numFvFaceColor
    INTEGER, ALLOCATABLE, DIMENSION(:) :: fvFaceColorX, fvFaceColorA

    INTEGER :: numFvCell
    INTEGER, ALLOCATABLE, DIMENSION(:) :: fvFaceToCellX, fvFaceToCellA

    CONTAINS

    SUBROUTINE setDualGraphEdgeColor

#define __MAX_COLOR_NUM__ 1024

        USE o_MESH,   ONLY: edge2D_in, edge_tri
        USE g_PARSUP, ONLY: myDim_edge2D, eDim_edge2D

        IMPLICIT NONE

        INTEGER :: numEdges, numActiveEdges, numProcessEdges
        LOGICAL, ALLOCATABLE :: edgeMask(:)

        INTEGER :: numElementLinks
        INTEGER :: minIdE, maxIdE
        INTEGER, ALLOCATABLE :: elementLink(:)
        LOGICAL, ALLOCATABLE :: elementMask(:)

        INTEGER :: idEdge, idColor
        INTEGER :: fvFCX(__MAX_COLOR_NUM__ + 1)

        WRITE(*,*) '------------------------------------------------------'
        WRITE(*,*) 'RUN SUBROUTINE setDualGraphEdgeColor'
        WRITE(*,*)
        WRITE(*,*) 'Warning! This subroutine should be used primarily for'
        WRITE(*,*) 'debugging purposes. The face-coloring algorithm'
        WRITE(*,*) 'implemented here is not optimal and, in some cases,'
        WRITE(*,*) 'produces an excessive number of colors, which'
        WRITE(*,*) 'negatively affects performance. In addition, to'
        WRITE(*,*) 'improve performance, faces should be sorted according'
        WRITE(*,*) 'to their color-group assignments.'        
        WRITE(*,*)

#ifdef USE_MPI
        numEdges = myDim_edge2D + eDim_edge2D
#else
        numEdges = edge2D_in
#endif

        ALLOCATE(edgeMask(1:numEdges))
        edgeMask(1:numEdges) = .FALSE.

        numActiveEdges = 0
        DO idEdge = 1, numEdges
#ifdef USE_MPI
            IF(myList_edge2D(idEdge) > edge2D_in) CYCLE
#endif
            numActiveEdges = numActiveEdges + 1
            edgeMask(idEdge) = .TRUE.

            IF(numActiveEdges == 1) THEN
                minIdE = edge_tri(1,idEdge)
                maxIdE = edge_tri(1,idEdge)
            END IF

            IF(minIdE > edge_tri(1,idEdge)) minIdE = edge_tri(1,idEdge)
            IF(maxIdE < edge_tri(1,idEdge)) maxIdE = edge_tri(1,idEdge)
            IF(minIdE > edge_tri(2,idEdge)) minIdE = edge_tri(2,idEdge)
            IF(maxIdE < edge_tri(2,idEdge)) maxIdE = edge_tri(2,idEdge)
        END DO

        WRITE(*,*) 'NUMBER OF        FACES: ', numEdges
        WRITE(*,*) 'NUMBER OF ACTIVE FACES: ', numActiveEdges
        WRITE(*,*) '        MIN ELEMENT ID: ', minIdE
        WRITE(*,*) '        MAX ELEMENT ID: ', maxIdE
        WRITE(*,*)

        ALLOCATE(elementMask(1:maxIdE), elementLink(1:maxIdE))
        elementLink(1:maxIdE) = 0

        DO idEdge = 1, numEdges
            IF(edgeMask(idEdge)) THEN
                elementLink(edge_tri(1,idEdge)) = elementLink(edge_tri(1,idEdge)) + 1
                elementLink(edge_tri(2,idEdge)) = elementLink(edge_tri(2,idEdge)) + 1
            END IF
        END DO
        numElementLinks = maxval(elementLink(1:maxIdE))
        WRITE(*,*) 'MAX NUMBER OF FACES PER FV: ', numElementLinks
        WRITE(*,*)

        ALLOCATE(fvFaceColorA(1:numActiveEdges))
        fvFCX(:)                       = -1
        fvFaceColorA(1:numActiveEdges) = -1

        WRITE(*,*) 'RUN MAIN LOOP'
        WRITE(*,*)

        numProcessEdges = 0; idColor = 0
        fvFCX(1) = 1
        DO WHILE (numProcessEdges /= numActiveEdges)

            IF(idColor > __MAX_COLOR_NUM__) THEN
                WRITE(*,*) 'Error! The current number of colors exceeds the'
                WRITE(*,*) 'predefined limit __MAX_COLOR_NUM__. With high'
                WRITE(*,*) 'probability, this indicates incorrect operation of'
                WRITE(*,*) 'the algorithm or errors in the input data.'                
                STOP
            END IF

            idColor = idColor + 1
            elementMask(1:maxIdE) = .TRUE.
            fvFCX(idColor + 1) = fvFCX(idColor)

            DO idEdge = 1, numEdges

                IF(edgeMask(idEdge)) THEN

                    IF( elementMask(edge_tri(1,idEdge)) .AND. elementMask(edge_tri(2,idEdge)) ) THEN
                        fvFaceColorA(fvFCX(idColor + 1)) = idEdge
                        fvFCX(idColor + 1) = fvFCX(idColor + 1) + 1

                        elementMask(edge_tri(1,idEdge)) = .FALSE.
                        elementLink(edge_tri(1,idEdge)) = elementLink(edge_tri(1,idEdge)) - 1
                        elementMask(edge_tri(2,idEdge)) = .FALSE.
                        elementLink(edge_tri(2,idEdge)) = elementLink(edge_tri(2,idEdge)) - 1

                        edgeMask(idEdge) = .FALSE.
                        numProcessEdges = numProcessEdges + 1
                    END IF

                END IF

            END DO ! idEdge

            WRITE(*,'(A,I5,A,I12)') 'COLOR ', idColor, ' FACES PROCESSED ', numProcessEdges

        END DO ! idColor

        WRITE(*,*)
        WRITE(*,*) 'FINAL TABLE OF FACE DISTRIBUTION ACROSS COLOR GROUPS'
        WRITE(*,'(*(I0,1X))') fvFCX(1:idColor+1)
        WRITE(*,*)

        IF(numProcessEdges /= numActiveEdges) THEN
            WRITE(*,*) 'Error! The number of processed faces does not match the number of active faces.'
            STOP
        END IF
        IF( (0 /= maxval(elementLink(1:maxIdE))) .OR. (0 /= minval(elementLink(1:maxIdE))) ) THEN
            WRITE(*,*) 'Error! Face lists are not constructed for all cells.'
            STOP
        END IF

        numFvFaceColor = idColor
        ALLOCATE(fvFaceColorX(1:numFvFaceColor+1))
        fvFaceColorX(1:numFvFaceColor+1) = fvFCX(1:numFvFaceColor+1)

        DEALLOCATE(edgeMask, elementMask, elementLink)

        WRITE(*,*)
        WRITE(*,*) 'FINISH SUBROUTINE setDualGraphEdgeColor'
        WRITE(*,*) '------------------------------------------------------'

    #undef __MAX_COLOR_NUM__

    END SUBROUTINE setDualGraphEdgeColor

    SUBROUTINE initFvFaceToCellMap

        USE o_MESH,   ONLY: edge2D_in, edge_tri
        USE g_PARSUP, ONLY: myDim_edge2D, eDim_edge2D

        IMPLICIT NONE

        INTEGER :: numEdges, numActiveEdges
        LOGICAL, ALLOCATABLE :: edgeMask(:)
        
        INTEGER :: minIdE, maxIdE

        INTEGER :: idL, idR
        INTEGER :: idEdge, idCell, idLink

        WRITE(*,*) '------------------------------------------------------'
        WRITE(*,*) 'RUN SUBROUTINE initFvFaceToCellMap'
        WRITE(*,*)

#ifdef USE_MPI
        numEdges = myDim_edge2D + eDim_edge2D
#else
        numEdges = edge2D_in
#endif

        ALLOCATE(edgeMask(1:numEdges))
        edgeMask(1:numEdges) = .FALSE.

        numActiveEdges = 0
        DO idEdge = 1, numEdges
#ifdef USE_MPI
            IF(myList_edge2D(idEdge) > edge2D_in) CYCLE
#endif
            numActiveEdges = numActiveEdges + 1
            edgeMask(idEdge) = .TRUE.

            IF(numActiveEdges == 1) THEN
                minIdE = edge_tri(1,idEdge)
                maxIdE = edge_tri(1,idEdge)
            END IF

            IF(minIdE > edge_tri(1,idEdge)) minIdE = edge_tri(1,idEdge)
            IF(maxIdE < edge_tri(1,idEdge)) maxIdE = edge_tri(1,idEdge)
            IF(minIdE > edge_tri(2,idEdge)) minIdE = edge_tri(2,idEdge)
            IF(maxIdE < edge_tri(2,idEdge)) maxIdE = edge_tri(2,idEdge)
        END DO

        WRITE(*,*) 'NUMBER OF        FACES: ', numEdges
        WRITE(*,*) 'NUMBER OF ACTIVE FACES: ', numActiveEdges
        WRITE(*,*) '        MIN ELEMENT ID: ', minIdE
        WRITE(*,*) '        MAX ELEMENT ID: ', maxIdE

        numFvCell = maxIdE
        ALLOCATE(fvFaceToCellX(1:numFvCell+1), fvFaceToCellA(1:numActiveEdges*2))
        fvFaceToCellX(:) = 0; fvFaceToCellA(:) = 0

        DO idEdge = 1, numEdges
            IF(edgeMask(idEdge)) THEN
                fvFaceToCellX(edge_tri(1,idEdge) + 1) = fvFaceToCellX(edge_tri(1,idEdge) + 1) + 1
                fvFaceToCellX(edge_tri(2,idEdge) + 1) = fvFaceToCellX(edge_tri(2,idEdge) + 1) + 1
            END IF
        END DO

        DO idCell = 3, numFvCell + 1
            fvFaceToCellX(idCell) = fvFaceToCellX(idCell) + fvFaceToCellX(idCell - 1)
        END DO
        IF(fvFaceToCellX(numFvCell + 1) /= numActiveEdges*2) THEN
            WRITE(*,*) 'Error! Offset array check failed (#1).'
            STOP
        END IF
        DO idCell = 1, numFvCell + 1
            fvFaceToCellX(idCell) = fvFaceToCellX(idCell) + 1
        END DO

        DO idEdge = 1, numEdges
            IF(edgeMask(idEdge)) THEN

                idL = edge_tri(1,idEdge)
                idR = edge_tri(2,idEdge)

                fvFaceToCellA(fvFaceToCellX(idL)) = - idEdge
                fvFaceToCellX(idL) = fvFaceToCellX(idL) + 1

                fvFaceToCellA(fvFaceToCellX(idR)) =   idEdge
                fvFaceToCellX(idR) = fvFaceToCellX(idR) + 1
                
            END IF
        END DO

        IF(fvFaceToCellX(numFvCell) /= numActiveEdges*2 + 1) THEN
            WRITE(*,*) 'Error! Offset array check failed (#2).'
            STOP
        END IF

        DO idEdge = 1, numActiveEdges*2
            IF(fvFaceToCellA(idEdge) == 0) THEN
                WRITE(*,*) 'Error! Face index array check failed (#3).'
                STOP
            END IF
        END DO

        DO idCell = numFvCell + 1, 2, -1
            fvFaceToCellX(idCell) = fvFaceToCellX(idCell - 1)
        END DO
        fvFaceToCellX(1) = 1

        DEALLOCATE(edgeMask)

        DO idCell = 1, numFvCell
            DO idLink = fvFaceToCellX(idCell), fvFaceToCellX(idCell + 1) - 1

                IF(fvFaceToCellA(idLink) < 0) THEN
                    IF(edge_tri(1, - fvFaceToCellA(idLink)) /= idCell) THEN
                        WRITE(*,*) 'Error! Cell-face assignment check failed (#4).'
                        STOP
                    END IF
                END IF

                IF(fvFaceToCellA(idLink) > 0) THEN
                    IF(edge_tri(2, fvFaceToCellA(idLink)) /= idCell) THEN
                        WRITE(*,*) 'Error! Cell-face assignment check failed (#5).'
                        STOP
                    END IF
                END IF

            END DO
        END DO
        
        WRITE(*,*)
        WRITE(*,*) 'FINISH SUBROUTINE initFvFaceToCellMap'
        WRITE(*,*) '------------------------------------------------------'

    END SUBROUTINE initFvFaceToCellMap

END MODULE graphProcessing
