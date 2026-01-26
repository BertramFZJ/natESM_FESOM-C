program main

#if 0
    USE timerLibFortran, ONLY: tlfInitTimerSpace, tlfNewSingleTimer, tlfStartSingleTimer, &
                            &  tlfStopSingleTimer, tlfPrintSingleTimerStatus, tlfInitIntegralTimerSpace, &
                            &  tlfNewIntegralTimer, tlfAttachCoreTimerToIntegral, &
                            &  tlfPrintIntegralTimerMembers, tlfUpdateIntegralTimer, &
                            &  tlfPrintIntegralTimerStatus
    USE timerLibFortran, ONLY: tlfUpdateIntegralTimerMPI, tlfPrintIntegralTimerStatusMPI
#else
    USE timerLibFortran
#endif

    IMPLICIT NONE

    include 'mpif.h'
    INTEGER :: mpiRank, mpiSize, ierror
    
    INTEGER, PARAMETER :: nRow = 9880
    INTEGER, PARAMETER :: nCol = 9550
    
    INTEGER, PARAMETER :: nIter = 10

    DOUBLE PRECISION, PARAMETER :: argStep = 0.001

    DOUBLE PRECISION, POINTER :: arg2D(:,:) => null() ! arg2D(1:nRow, 1:nCol)
    DOUBLE PRECISION, POINTER :: funcA(:,:) => null() ! funcA(1:nRow, 1:nCol)
    DOUBLE PRECISION, POINTER :: funcB(:,:) => null() ! funcB(1:nRow, 1:nCol)
    DOUBLE PRECISION, POINTER :: funcT(:,:) => null() ! funcB(1:nRow, 1:nCol)

    DOUBLE PRECISION :: argOffset
    INTEGER :: idIter, col, row

    INTEGER :: idTimerGlobal, idTimerAllocate, idTimerStageArg
    INTEGER :: idTimerStageFA, idTimerStageFB, idTimerStageFT
    INTEGER :: idIntegralTimerML

    DOUBLE PRECISION :: globalTimerTotal, integralTimerTotal
    INTEGER          :: globalTimerCallNumber, integralTimerNumPE

    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpiSize, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, mpiRank, ierror)

    WRITE(*,*) "************** PROGRAM START  POINT"

    CALL tlfInitTimerSpace()
    idTimerGlobal   = tlfNewSingleTimer("Global Loop Timer")
    idTimerAllocate = tlfNewSingleTimer("Allocate Timer")
    idTimerStageArg = tlfNewSingleTimer("Stage Arg Timer")
    idTimerStageFA  = tlfNewSingleTimer("Stage FA Timer")
    idTimerStageFB  = tlfNewSingleTimer("Stage FB Timer")
    idTimerStageFT  = tlfNewSingleTimer("Stage FT Timer")
    WRITE(*,*)

    CALL tlfInitIntegralTimerSpace()
    idIntegralTimerML = tlfNewIntegralTimer("Main Loop Integral Timer")
    CALL tlfAttachCoreTimerToIntegral(idTimerStageFA, idIntegralTimerML)
    CALL tlfAttachCoreTimerToIntegral(idTimerStageFB, idIntegralTimerML)
    CALL tlfAttachCoreTimerToIntegral(idTimerStageFT, idIntegralTimerML)
    WRITE(*,*)
    CALL tlfPrintIntegralTimerMembers(idIntegralTimerML)
    WRITE(*,*)
    
    CALL tlfStartSingleTimer(idTimerAllocate)
    ALLOCATE(arg2D(1:nRow, 1:nCol), funcA(1:nRow, 1:nCol))
    ALLOCATE(funcB(1:nRow, 1:nCol), funcT(1:nRow, 1:nCol))
    CALL tlfStopSingleTimer(idTimerAllocate)
    
    CALL tlfStartSingleTimer(idTimerGlobal)
    argOffset = 0.0
    DO idIter = 1, nIter

        ! 1. Инициализация значений аргументов
        CALL tlfStartSingleTimer(idTimerStageArg)
        DO col = 1, nCol
            DO row = 1, nRow
                arg2D(row, col) = argOffset
                argOffset = argOffset + argStep
            END DO
        END DO
        CALL tlfStopSingleTimer(idTimerStageArg)

        ! 2. Инициализация значений функции A
        CALL tlfStartSingleTimer(idTimerStageFA)
        DO col = 1, nCol
            DO row = 1, nRow
                funcA(row, col) = SIN(arg2D(row, col)) * SIN(arg2D(row, col))
            END DO
        END DO
        CALL tlfStopSingleTimer(idTimerStageFA)

        ! 3. Инициализация значений функции B
        CALL tlfStartSingleTimer(idTimerStageFB)
        DO col = 1, nCol
            DO row = 1, nRow
                funcB(row, col) = COS(arg2D(row, col))
                funcB(row, col) = funcB(row, col) * funcB(row, col)
            END DO
        END DO
        CALL tlfStopSingleTimer(idTimerStageFB)

        ! 4. Инициализация значений функции T
        CALL tlfStartSingleTimer(idTimerStageFT)
        DO col = 1, nCol
            DO row = 1, nRow
                funcT(row, col) = funcA(row, col) + funcB(row, col)                
            END DO
        END DO
        CALL tlfStopSingleTimer(idTimerStageFT)

        IF(mpiRank == 0) WRITE(*,*) "ITER: ", idIter
        ! WRITE(*,*) MINVAL(arg2D(:,:)), " ", MAXVAL(arg2D(:,:))
        ! WRITE(*,*) 1.0 - MINVAL(funcT(:,:)), " ", MAXVAL(funcT(:,:)) - 1.0

    END DO
    CALL tlfStopSingleTimer(idTimerGlobal)

    WRITE(*,*)
    CALL tlfPrintSingleTimerStatus(idTimerGlobal,   2)
    CALL tlfPrintSingleTimerStatus(idTimerAllocate, 1)
    CALL tlfPrintSingleTimerStatus(idTimerStageArg, 1)
    CALL tlfPrintSingleTimerStatus(idTimerStageFA,  1)
    CALL tlfPrintSingleTimerStatus(idTimerStageFB,  1)
    CALL tlfPrintSingleTimerStatus(idTimerStageFT,  1)
    WRITE(*,*)

    CALL tlfUpdateIntegralTimer(idIntegralTimerML)
    CALL tlfPrintIntegralTimerStatus(idIntegralTimerML, 2)
    CALL tlfUpdateIntegralTimerMPI(idIntegralTimerML, MPI_COMM_WORLD, .FALSE.)
    IF(mpiRank == 0) THEN
        CALL tlfPrintIntegralTimerStatusMPI(idIntegralTimerML)
    END IF
    WRITE(*,*)

    call MPI_BARRIER( MPI_COMM_WORLD, ierror)

    IF(mpiRank == 0) THEN
        globalTimerTotal = tlfGetSingleTimerFieldValueDP(idTimerGlobal, getCoreTimerTotalTime)
        globalTimerCallNumber = tlfGetSingleTimerFieldValueI(idTimerGlobal, getCoreTimerNumberOfCalls)
        WRITE(*,'(A,G12.5,A,I0)') 'PE 0: Global Timer Total = ', globalTimerTotal, ' nCall = ', globalTimerCallNumber
        integralTimerTotal = tlfGetIntegralTimerFieldValueDP(idIntegralTimerML, getIntegralTimerTotalTime)
        integralTimerNumPE = tlfGetIntegralTimerFieldValueI(idIntegralTimerML, getIntegralTimerNumPE)
        WRITE(*,'(A,G12.5,A,I0)') 'PE 0: Main Loop Integral Timer Total = ', integralTimerTotal, ' NumPE = ', integralTimerNumPE
        WRITE(*,*)
    END IF

    WRITE(*,*) "************** PROGRAM FINISH POINT"

    call MPI_BARRIER( MPI_COMM_WORLD, ierror)
    call MPI_FINALIZE(ierror)

end program main
