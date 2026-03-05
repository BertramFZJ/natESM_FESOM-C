MODULE memLibFortran

    USE o_PARAM

    IMPLICIT NONE

    PUBLIC :: allocateMemLibArrays

    PUBLIC :: memReal2D_01, memReal2D_02, memReal2D_03
    PUBLIC :: memReal1D_NODES_01
    PUBLIC :: memReal1D_NSIGMA_01 , memReal1D_NSIGMA_02
    PUBLIC :: memReal1D_NSIGMA1_01, memReal1D_NSIGMA1_02, memReal1D_NSIGMA1_03

    PRIVATE

    REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:), TARGET :: memReal2D_01, memReal2D_02, memReal2D_03
    REAL(KIND=WP), ALLOCATABLE, DIMENSION(:)  , TARGET :: memReal1D_NODES_01
    REAL(KIND=WP), ALLOCATABLE, DIMENSION(:)  , TARGET :: memReal1D_NSIGMA_01, memReal1D_NSIGMA_02
    REAL(KIND=WP), ALLOCATABLE, DIMENSION(:)  , TARGET :: memReal1D_NSIGMA1_01, memReal1D_NSIGMA1_02, &
                                                          memReal1D_NSIGMA1_03

    CONTAINS

    SUBROUTINE allocateMemLibArrays

        USE g_PARSUP

        IMPLICIT NONE

        INTEGER(KIND=8) :: totalMemSizeByte
        REAL(KIND=8)    :: totalMemSizeMByte
        INTEGER(KIND=8) :: realVariableSize
        INTEGER(KIND=4) :: numOfArrays

        WRITE(*,*) '------------------------------------------------------'
        WRITE(*,*) 'RUN SUBROUTINE allocateMemLibArrays'
        WRITE(*,*)

        numOfArrays = 0
        totalMemSizeByte = 0_8
        realVariableSize = 0_8
        IF(WP == 8) realVariableSize = 8_8
        IF(WP == 4) realVariableSize = 4_8
        IF(realVariableSize == 0) THEN
            WRITE(*,*) 'Error! The size of real variables is not defined.'
            STOP
        END IF

        ALLOCATE(memReal1D_NODES_01(myDim_nod2D + eDim_nod2D))
        totalMemSizeByte = totalMemSizeByte + SIZE(memReal1D_NODES_01, KIND=8) * realVariableSize
        numOfArrays = numOfArrays + 1

        ALLOCATE(memReal2D_01(nsigma-1, myDim_nod2D + eDim_nod2D))
        totalMemSizeByte = totalMemSizeByte + SIZE(memReal2D_01, KIND=8) * realVariableSize
        numOfArrays = numOfArrays + 1
        ALLOCATE(memReal2D_02(nsigma-1, myDim_nod2D + eDim_nod2D))
        totalMemSizeByte = totalMemSizeByte + SIZE(memReal2D_02, KIND=8) * realVariableSize
        numOfArrays = numOfArrays + 1
        ALLOCATE(memReal2D_03(nsigma-1, myDim_nod2D + eDim_nod2D))
        totalMemSizeByte = totalMemSizeByte + SIZE(memReal2D_03, KIND=8) * realVariableSize
        numOfArrays = numOfArrays + 1

#if 0
        ALLOCATE(memReal1D_NSIGMA_01(nsigma), memReal1D_NSIGMA_02(nsigma))

        ALLOCATE(memReal1D_NSIGMA1_01(nsigma-1), memReal1D_NSIGMA1_02(nsigma-1))
        ALLOCATE(memReal1D_NSIGMA1_03(nsigma-1))
#endif

        totalMemSizeMByte = REAL(totalMemSizeByte,8) / (1024.0_8 * 1024.0_8)
        WRITE(*,'(1X,A,I12)')        'Total number of allocated arrays: ', numOfArrays
        WRITE(*,'(1X,A,I12,1X,A)')   'Total   size of allocated arrays: ', totalMemSizeByte, 'bytes'
        WRITE(*,'(1X,A,F12.3,1X,A)') 'Total   size of allocated arrays: ', totalMemSizeMByte, 'MB'

        WRITE(*,*)
        WRITE(*,*) 'FINISH SUBROUTINE allocateMemLibArrays'
        WRITE(*,*) '------------------------------------------------------'

    END SUBROUTINE allocateMemLibArrays

END MODULE memLibFortran
