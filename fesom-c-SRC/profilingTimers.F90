MODULE profilingTimers

    USE g_PARSUP, ONLY: mype, npes, MPI_COMM_FESOM_C
    USE timerLibFortran

    IMPLICIT NONE

#ifdef USE_MPI
    include 'mpif.h'
#endif

    PRIVATE

    PUBLIC :: idMainLoopTimer, idDataExchangeTimer, idDataBroadcastTimer, idDataGatherTimer

    PUBLIC :: ptInitTimerSpace
    PUBLIC :: ptRebootTimerSpace
    PUBLIC :: ptAnalizeControlPoint

    INTEGER :: idMainLoopTimer
    INTEGER :: idDataExchangeTimer
    INTEGER :: idDataBroadcastTimer
    INTEGER :: idDataGatherTimer

    CONTAINS

    SUBROUTINE ptInitTimerSpace

        IMPLICIT NONE

        CALL tlfInitTimerSpace()

        idMainLoopTimer      = tlfNewSingleTimer("Main Loop Timer")
        idDataExchangeTimer  = tlfNewSingleTimer("Data Exchange Timer")
        idDataBroadcastTimer = tlfNewSingleTimer("Data Broadcast Timer")
        idDataGatherTimer    = tlfNewSingleTimer("Data Gather Timer")

    END SUBROUTINE ptInitTimerSpace

    SUBROUTINE ptRebootTimerSpace

        IMPLICIT NONE

        CALL tlfRebootSingleTimer(idMainLoopTimer)
        CALL tlfRebootSingleTimer(idDataExchangeTimer)
        CALL tlfRebootSingleTimer(idDataBroadcastTimer)
        CALL tlfRebootSingleTimer(idDataGatherTimer)

    END SUBROUTINE ptRebootTimerSpace

    SUBROUTINE ptAnalizeControlPoint

        IMPLICIT NONE

        DOUBLE PRECISION :: totalTime, calcTime
        DOUBLE PRECISION :: exchTime, bcTime, gatherTime, overheadTime
#ifdef USE_MPI
        DOUBLE PRECISION :: mpiMinCalcTime, mpiAvgCalcTime, mpiMaxCalcTime
        INTEGER          :: ierror
#endif

        totalTime = tlfGetSingleTimerFieldValueDP(idMainLoopTimer, getCoreTimerTotalTime)

        exchTime   = tlfGetSingleTimerFieldValueDP(idDataExchangeTimer,  getCoreTimerTotalTime)
        bcTime     = tlfGetSingleTimerFieldValueDP(idDataBroadcastTimer, getCoreTimerTotalTime)
        gatherTime = tlfGetSingleTimerFieldValueDP(idDataGatherTimer,    getCoreTimerTotalTime)
        overheadTime = exchTime + bcTime + gatherTime

        calcTime = totalTime - overheadTime

        WRITE(*,'(A,I6,A,G12.5,A,G12.5,A,F6.3)') 'PE ', mype, ' ***TCP***: CALC = ', calcTime, &
        ' OVERHEAD = ', overheadTime, ' TRANSFER SHARE = ', overheadTime / calcTime

#ifdef USE_MPI
        CALL MPI_Allreduce(calcTime, mpiMinCalcTime, 1, MPI_DOUBLE_PRECISION, MPI_MIN, MPI_COMM_FESOM_C, ierror)
        CALL MPI_Allreduce(calcTime, mpiMaxCalcTime, 1, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_FESOM_C, ierror)
        CALL MPI_Allreduce(calcTime, mpiAvgCalcTime, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_FESOM_C, ierror)
        mpiAvgCalcTime = mpiAvgCalcTime / DBLE(npes)
        IF(mype == 0) THEN
            WRITE(*,'(A,G12.5,A,G12.5,A,G12.5,A,G12.5)') '***TCP***: CALC DIST = [', mpiMinCalcTime, '; ', &
            mpiAvgCalcTime, '; ', mpiMaxCalcTime, '] >> ', mpiMaxCalcTime / mpiAvgCalcTime
        END IF
#endif

    END SUBROUTINE ptAnalizeControlPoint

END MODULE profilingTimers
