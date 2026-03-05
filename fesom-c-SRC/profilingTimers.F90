MODULE profilingTimers

    USE g_PARSUP, ONLY: mype, npes, MPI_COMM_FESOM_C
    USE timerLibFortran

    IMPLICIT NONE

#ifdef USE_MPI
    include 'mpif.h'
#endif

    PRIVATE

    PUBLIC :: idMainLoopTimer, idDataExchangeTimer, idDataBroadcastTimer, idDataGatherTimer
    PUBLIC :: id_momentum_vert_adv_upwind, id_momentum_vert_adv_upwind_UP
    PUBLIC :: id_momentum_adv_P1_3D_to_2D, id_momentum_adv_P1_3D_to_2D_UP
    PUBLIC :: id_momentum_adv_upwind_2D, id_momentum_adv_upwind_2D_UP
    PUBLIC :: id_momentum_adv_upwind, id_momentum_adv_upwind_UP
    PUBLIC :: id_solve_tracer_upwind, id_solve_tracer_upwind_UP

    PUBLIC :: ptInitTimerSpace
    PUBLIC :: ptRebootTimerSpace
    PUBLIC :: ptAnalizeControlPoint
    PUBLIC :: ptInitAdvectionTimerGroup
    PUBLIC :: ptRebootAdvectionTimerGroup
    PUBLIC :: ptAnalizeAdvectionTimerGroup

    INTEGER :: idMainLoopTimer
    INTEGER :: idDataExchangeTimer
    INTEGER :: idDataBroadcastTimer
    INTEGER :: idDataGatherTimer

    INTEGER :: id_momentum_vert_adv_upwind, id_momentum_vert_adv_upwind_UP
    INTEGER :: id_momentum_adv_P1_3D_to_2D, id_momentum_adv_P1_3D_to_2D_UP
    INTEGER :: id_momentum_adv_upwind_2D, id_momentum_adv_upwind_2D_UP
    INTEGER :: id_momentum_adv_upwind, id_momentum_adv_upwind_UP

    INTEGER :: id_solve_tracer_upwind, id_solve_tracer_upwind_UP

    CONTAINS

    SUBROUTINE ptInitTimerSpace

        IMPLICIT NONE

        CALL tlfInitTimerSpace()

        idMainLoopTimer      = tlfNewSingleTimer("Main Loop Timer")
        idDataExchangeTimer  = tlfNewSingleTimer("Data Exchange Timer")
        idDataBroadcastTimer = tlfNewSingleTimer("Data Broadcast Timer")
        idDataGatherTimer    = tlfNewSingleTimer("Data Gather Timer")

    END SUBROUTINE ptInitTimerSpace

    SUBROUTINE ptInitAdvectionTimerGroup

        IMPLICIT NONE

        id_momentum_vert_adv_upwind    = tlfNewSingleTimer("FV ADV momentum_vert_adv_upwind INTERNAL")
        id_momentum_vert_adv_upwind_UP = tlfNewSingleTimer("FV ADV momentum_vert_adv_upwind EXTERNAL")

        id_momentum_adv_P1_3D_to_2D    = tlfNewSingleTimer("FV ADV momentum_adv_P1_3D_to_2D INTERNAL")
        id_momentum_adv_P1_3D_to_2D_UP = tlfNewSingleTimer("FV ADV momentum_adv_P1_3D_to_2D EXTERNAL")

        id_momentum_adv_upwind_2D      = tlfNewSingleTimer("FV ADV momentum_adv_upwind_2D INTERNAL")
        id_momentum_adv_upwind_2D_UP   = tlfNewSingleTimer("FV ADV momentum_adv_upwind_2D EXTERNAL")

        id_momentum_adv_upwind         = tlfNewSingleTimer("FV ADV momentum_adv_upwind INTERNAL")
        id_momentum_adv_upwind_UP      = tlfNewSingleTimer("FV ADV momentum_adv_upwind EXTERNAL")

        id_solve_tracer_upwind         = tlfNewSingleTimer("FV TRACER solve_tracer_upwind INTERNAL")
        id_solve_tracer_upwind_UP      = tlfNewSingleTimer("FV TRACER solve_tracer_upwind EXTERNAL")

    END SUBROUTINE ptInitAdvectionTimerGroup

    SUBROUTINE ptRebootTimerSpace

        IMPLICIT NONE

        CALL tlfRebootSingleTimer(idMainLoopTimer)
        CALL tlfRebootSingleTimer(idDataExchangeTimer)
        CALL tlfRebootSingleTimer(idDataBroadcastTimer)
        CALL tlfRebootSingleTimer(idDataGatherTimer)

    END SUBROUTINE ptRebootTimerSpace

    SUBROUTINE ptRebootAdvectionTimerGroup

        IMPLICIT NONE

        CALL tlfRebootSingleTimer(id_momentum_vert_adv_upwind)
        CALL tlfRebootSingleTimer(id_momentum_vert_adv_upwind_UP)

        CALL tlfRebootSingleTimer(id_momentum_adv_P1_3D_to_2D)
        CALL tlfRebootSingleTimer(id_momentum_adv_P1_3D_to_2D_UP)

        CALL tlfRebootSingleTimer(id_momentum_adv_upwind_2D)
        CALL tlfRebootSingleTimer(id_momentum_adv_upwind_2D_UP)

        CALL tlfRebootSingleTimer(id_momentum_adv_upwind)
        CALL tlfRebootSingleTimer(id_momentum_adv_upwind_UP)

        CALL tlfRebootSingleTimer(id_solve_tracer_upwind)
        CALL tlfRebootSingleTimer(id_solve_tracer_upwind_UP)

    END SUBROUTINE ptRebootAdvectionTimerGroup

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

    SUBROUTINE ptAnalizeAdvectionTimerGroup

        IMPLICIT NONE

        CALL tlfPrintSingleTimerStatus(id_momentum_vert_adv_upwind,    2)
        CALL tlfPrintSingleTimerStatus(id_momentum_vert_adv_upwind_UP, 1)

        CALL tlfPrintSingleTimerStatus(id_momentum_adv_P1_3D_to_2D,    1)
        CALL tlfPrintSingleTimerStatus(id_momentum_adv_P1_3D_to_2D_UP, 1)

        CALL tlfPrintSingleTimerStatus(id_momentum_adv_upwind_2D,      1)
        CALL tlfPrintSingleTimerStatus(id_momentum_adv_upwind_2D_UP,   1)

        CALL tlfPrintSingleTimerStatus(id_momentum_adv_upwind,         1)
        CALL tlfPrintSingleTimerStatus(id_momentum_adv_upwind_UP,      1)

        CALL tlfPrintSingleTimerStatus(id_solve_tracer_upwind,         1)
        CALL tlfPrintSingleTimerStatus(id_solve_tracer_upwind_UP,      1)

    END SUBROUTINE ptAnalizeAdvectionTimerGroup

END MODULE profilingTimers
