#define _TLF_USE_MPI_

MODULE timerLibFortran

    USE omp_lib, ONLY: omp_get_wtime

    IMPLICIT NONE

#ifdef _TLF_USE_MPI_
    include 'mpif.h'
#endif

    PRIVATE

    PUBLIC :: getCoreTimerTotalTime
    PUBLIC :: getCoreTimerNumberOfCalls
    PUBLIC :: getIntegralTimerTotalTime
    PUBLIC :: getIntegralTimerNumPE

    PUBLIC :: tlfInitTimerSpace
    PUBLIC :: tlfNewSingleTimer
    PUBLIC :: tlfStartSingleTimer
    PUBLIC :: tlfStopSingleTimer
    PUBLIC :: tlfGetSingleTimerFieldValueDP, tlfGetSingleTimerFieldValueI
    PUBLIC :: tlfPrintSingleTimerStatus
    PUBLIC :: tlfInitIntegralTimerSpace
    PUBLIC :: tlfNewIntegralTimer
    PUBLIC :: tlfAttachCoreTimerToIntegral
    PUBLIC :: tlfPrintIntegralTimerMembers
    PUBLIC :: tlfUpdateIntegralTimer
    PUBLIC :: tlfPrintIntegralTimerStatus
    PUBLIC :: tlfGetIntegralTimerFieldValueDP, tlfGetIntegralTimerFieldValueI
#ifdef _TLF_USE_MPI_
    PUBLIC :: tlfUpdateIntegralTimerMPI
    PUBLIC :: tlfPrintIntegralTimerStatusMPI
#endif

    INTEGER, PARAMETER :: timerSpaceSize = 64
    INTEGER            :: numActiveTimers
    INTEGER, PARAMETER :: integralTimerSpaceSize = 32
    INTEGER            :: numActiveIntegralTimers

    INTEGER, PARAMETER :: getCoreTimerTotalTime = 1
    INTEGER, PARAMETER :: getCoreTimerNumberOfCalls = 1

    INTEGER, PARAMETER :: getIntegralTimerTotalTime = 1
    INTEGER, PARAMETER :: getIntegralTimerNumPE = 1

    TYPE tSingleTimerHeader

        INTEGER            :: idTimer

        LOGICAL            :: stat
        CHARACTER(len=128) :: name

    END TYPE tSingleTimerHeader

    TYPE tSingleTimerCore

        INTEGER :: idTimer

        DOUBLE PRECISION :: startMark

        DOUBLE PRECISION :: totT
        DOUBLE PRECISION :: minT
        DOUBLE PRECISION :: maxT
        DOUBLE PRECISION :: avgT
        DOUBLE PRECISION :: lastT

        INTEGER          :: nCall

        LOGICAL          :: stat

    END TYPE tSingleTimerCore

    TYPE tIntegralTimer

        INTEGER            :: idTimer

        LOGICAL            :: stat
        CHARACTER(len=128) :: name
        
        DOUBLE PRECISION   :: totT   
        DOUBLE PRECISION   :: lastT

        INTEGER            :: numMembers
        INTEGER            :: listOfSingleTimers(timerSpaceSize)

        INTEGER            :: numPE
        DOUBLE PRECISION   :: mpiTotT
        DOUBLE PRECISION   :: mpiMinTotT
        DOUBLE PRECISION   :: mpiMaxTotT
        DOUBLE PRECISION   :: mpiAvgTotT
        DOUBLE PRECISION   :: mpiLastT
        DOUBLE PRECISION   :: mpiMinLastT
        DOUBLE PRECISION   :: mpiMaxLastT
        DOUBLE PRECISION   :: mpiAvgLastT

    END TYPE tIntegralTimer

    TYPE(tSingleTimerHeader) ::  timerSpaceHeaders(timerSpaceSize        )
    TYPE(tSingleTimerCore)   ::    timerSpaceCores(timerSpaceSize        )
    TYPE(tIntegralTimer)     :: integralTimerSpace(integralTimerSpaceSize)

    CONTAINS

    SUBROUTINE tlfInitTimerSpace

        IMPLICIT NONE

        INTEGER :: iTimer

        numActiveTimers = 0

        DO iTimer = 1, timerSpaceSize

            timerSpaceHeaders(iTimer)%idTimer = - 1
            timerSpaceHeaders(iTimer)%stat    = .FALSE.
            timerSpaceHeaders(iTimer)%name    = TRIM('NO NAME')

            timerSpaceCores(iTimer)%idTimer   = -1
            timerSpaceCores(iTimer)%startMark = 0.0D0
            timerSpaceCores(iTimer)%totT      = 0.0D0
            timerSpaceCores(iTimer)%minT      = 0.0D0
            timerSpaceCores(iTimer)%maxT      = 0.0D0
            timerSpaceCores(iTimer)%avgT      = 0.0D0
            timerSpaceCores(iTimer)%lastT     = 0.0D0
            timerSpaceCores(iTimer)%nCall     = 0
            timerSpaceCores(iTimer)%stat      = .FALSE.

        END DO

        WRITE(*,*) "COMPLETE: tlfInitTimerSpace"

    END SUBROUTINE tlfInitTimerSpace

    FUNCTION tlfNewSingleTimer(name, idTimerIn) result(idTimerOut)

        IMPLICIT NONE

        CHARACTER(len=*), INTENT(in), OPTIONAL :: name
        INTEGER,          INTENT(in), OPTIONAL :: idTimerIn
        INTEGER                                :: idTimerOut

        idTimerOut = -1
        IF(PRESENT(idTimerIn)) THEN
            IF(timerSpaceHeaders(idTimerIn)%stat .EQV. .FALSE.) THEN
                idTimerOut = idTimerIn
            ELSE                
                WRITE(*, '(A,I0,A)') "ERROR => tlfNewSingleTimer: Timer with id = ", idTimerIn, " already exists."
                ERROR STOP
            END IF
        END IF

        IF(idTimerOut == -1) THEN

            IF(numActiveTimers == timerSpaceSize) THEN
                ERROR STOP "ERROR => tlfNewSingleTimer: The limit for the maximum number of timers was exceeded."
            END IF

            DO idTimerOut = 1, timerSpaceSize
                IF(timerSpaceHeaders(idTimerOut)%stat .EQV. .FALSE.) THEN
                    EXIT
                END IF
            END DO

            IF(idTimerOut == timerSpaceSize) THEN
                ERROR STOP "ERROR => tlfNewSingleTimer: Unable to assign an id to the new timer."
            END IF

        END IF

        timerSpaceHeaders(idTimerOut)%name = ""
        IF(PRESENT(name)) THEN            
            IF(len_trim(name) > 0) THEN
                timerSpaceHeaders(idTimerOut)%name = name(1:min(128, len_trim(name)))
            END IF
        ENDIF

        timerSpaceHeaders(idTimerOut)%idTimer = idTimerOut
        timerSpaceHeaders(idTimerOut)%stat    = .TRUE.
          timerSpaceCores(idTimerOut)%idTimer = idTimerOut

        numActiveTimers = numActiveTimers + 1

        WRITE(*,'(A,I0,A,A,A)') 'tlfNewSingleTimer: Created new timer with id=', idTimerOut, &
        & ', name="', trim(timerSpaceHeaders(idTimerOut)%name), '"'

    END FUNCTION tlfNewSingleTimer

    SUBROUTINE tlfStartSingleTimer(idTimer)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idTimer

        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfStartSingleTimer: Timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(timerSpaceCores(idTimer)%stat .EQV. .TRUE.) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfStartSingleTimer: Timer with id=", idTimer, " is already running."
            ERROR STOP
        END IF

        timerSpaceCores(idTimer)%startMark = omp_get_wtime()
        timerSpaceCores(idTimer)%stat = .TRUE.

    END SUBROUTINE tlfStartSingleTimer

    SUBROUTINE tlfStopSingleTimer(idTimer)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idTimer
        
        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfStopSingleTimer: Timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(timerSpaceCores(idTimer)%stat .EQV. .FALSE.) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfStopSingleTimer: Timer with id=", idTimer, " was not started."
            ERROR STOP
        END IF

        timerSpaceCores(idTimer)%lastT = omp_get_wtime() - timerSpaceCores(idTimer)%startMark
        timerSpaceCores(idTimer)%stat = .FALSE.
        timerSpaceCores(idTimer)%totT = timerSpaceCores(idTimer)%totT + timerSpaceCores(idTimer)%lastT

        IF(timerSpaceCores(idTimer)%nCall == 0) THEN
            timerSpaceCores(idTimer)%nCall = 1
            timerSpaceCores(idTimer)%minT = timerSpaceCores(idTimer)%lastT
            timerSpaceCores(idTimer)%maxT = timerSpaceCores(idTimer)%lastT
            timerSpaceCores(idTimer)%avgT = timerSpaceCores(idTimer)%lastT

        ELSE
            timerSpaceCores(idTimer)%nCall = timerSpaceCores(idTimer)%nCall + 1
            timerSpaceCores(idTimer)%minT = min(timerSpaceCores(idTimer)%minT, timerSpaceCores(idTimer)%lastT)
            timerSpaceCores(idTimer)%maxT = max(timerSpaceCores(idTimer)%maxT, timerSpaceCores(idTimer)%lastT)
            timerSpaceCores(idTimer)%avgT = timerSpaceCores(idTimer)%totT / DBLE(timerSpaceCores(idTimer)%nCall)
        END IF

    END SUBROUTINE tlfStopSingleTimer

    FUNCTION tlfGetSingleTimerFieldValueDP(idTimer, idField) result(fieldValue)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idTimer
        INTEGER, INTENT(in) :: idField
        DOUBLE PRECISION    :: fieldValue

        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN
            WRITE(*, '(A,I0,A)') "ERROR => tlfGetSingleTimerFieldValueDP: Timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(idField == getCoreTimerTotalTime) THEN
            fieldValue = timerSpaceCores(idTimer)%totT
            RETURN
        END IF

        WRITE(*, '(A,I0,A)') "ERROR => tlfGetSingleTimerFieldValueDP: Field id=", idField, " is not supported."
        ERROR STOP

    END FUNCTION tlfGetSingleTimerFieldValueDP

    FUNCTION tlfGetSingleTimerFieldValueI(idTimer, idField) result(fieldValue)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idTimer
        INTEGER, INTENT(in) :: idField
        INTEGER             :: fieldValue

        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN
            WRITE(*, '(A,I0,A)') "ERROR => tlfGetSingleTimerFieldValueI: Timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(idField == getCoreTimerNumberOfCalls) THEN
            fieldValue = timerSpaceCores(idTimer)%nCall
            RETURN
        END IF

        WRITE(*, '(A,I0,A)') "ERROR => tlfGetSingleTimerFieldValueI: Field id=", idField, " is not supported."
        ERROR STOP

    END FUNCTION tlfGetSingleTimerFieldValueI

    SUBROUTINE tlfPrintSingleTimerStatus(idTimer, fmtIn)

        IMPLICIT NONE

        INTEGER, INTENT(in)           :: idTimer
        INTEGER, INTENT(in), OPTIONAL :: fmtIn

        INTEGER                       :: fmt


        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfPrintSingleTimerStatus: Timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(timerSpaceCores(idTimer)%stat .EQV. .TRUE.) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfPrintSingleTimerStatus: Timer with id=", idTimer, " is currently running."
            ERROR STOP
        END IF

        fmt = 0
        IF(PRESENT(fmtIn)) THEN
            IF(fmtIn == 1) fmt = 1 ! Вывод только строки с параметрами
            IF(fmtIn == 2) fmt = 2 ! Вывод заголовка + строки с параметрами
        END IF

        IF(fmt == 0) THEN
            WRITE(*,'(a)') 'Timer information:'
            WRITE(*,'(a,i12)') '  ID            : ', timerSpaceHeaders(idTimer)%idTimer
            WRITE(*,'(a,a)')  '  Name          : ', trim(timerSpaceHeaders(idTimer)%name)
            WRITE(*,'(a,i12)') '  Calls         : ', timerSpaceCores(idTimer)%nCall
            WRITE(*,'(a)')    '  Time (s):'
            WRITE(*,'(a,G12.5)') '    Min         : ', timerSpaceCores(idTimer)%minT
            WRITE(*,'(a,G12.5)') '    Max         : ', timerSpaceCores(idTimer)%maxT
            WRITE(*,'(a,G12.5)') '    Avg         : ', timerSpaceCores(idTimer)%avgT
            WRITE(*,'(a,G12.5)') '    Total       : ', timerSpaceCores(idTimer)%totT
            WRITE(*,'(a,G12.5)') '    Last        : ', timerSpaceCores(idTimer)%lastT
        END IF

        IF(fmt /= 0) THEN

            IF(fmt == 2) THEN
                WRITE(*,'(a)') &
                '  ID    Calls       Min(s)       Max(s)       Avg(s)     Total(s)      Last(s) **** Name ****'
                WRITE(*,'(a)') &
                '---------------------------------------------------------------------------------------------'
            END IF

            WRITE(*, '(I4,1x,I8,1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5,1x,G12.5,1x,a,a,a)') &
            timerSpaceHeaders(idTimer)%idTimer, timerSpaceCores(idTimer)%nCall, &
            timerSpaceCores(idTimer)%minT, timerSpaceCores(idTimer)%maxT, timerSpaceCores(idTimer)%avgT, &
            timerSpaceCores(idTimer)%totT, timerSpaceCores(idTimer)%lastT, &
            '"', trim(timerSpaceHeaders(idTimer)%name), '"'
        END IF

    END SUBROUTINE tlfPrintSingleTimerStatus

    SUBROUTINE tlfInitIntegralTimerSpace

        IMPLICIT NONE

        INTEGER :: iTimer
        INTEGER :: i
        
        numActiveIntegralTimers = 0

        DO iTimer = 1, integralTimerSpaceSize

            integralTimerSpace(iTimer)%idTimer = -1
            integralTimerSpace(iTimer)%stat    = .FALSE.
            integralTimerSpace(iTimer)%name    = TRIM('NO NAME')

            integralTimerSpace(iTimer)%totT  = 0.0D0
            integralTimerSpace(iTimer)%lastT = 0.0D0

            integralTimerSpace(iTimer)%numMembers = 0
            DO i = 1, integralTimerSpaceSize
                integralTimerSpace(iTimer)%listOfSingleTimers(i) = -1
            END DO

            integralTimerSpace(iTimer)%numPE       = 0
            integralTimerSpace(iTimer)%mpiTotT     = 0.0D0
            integralTimerSpace(iTimer)%mpiMinTotT  = 0.0D0
            integralTimerSpace(iTimer)%mpiMaxTotT  = 0.0D0
            integralTimerSpace(iTimer)%mpiAvgTotT  = 0.0D0
            integralTimerSpace(iTimer)%mpiLastT    = 0.0D0
            integralTimerSpace(iTimer)%mpiMinLastT = 0.0D0
            integralTimerSpace(iTimer)%mpiMaxLastT = 0.0D0
            integralTimerSpace(iTimer)%mpiAvgLastT = 0.0D0

        END DO

        WRITE(*,*) "COMPLETE: tlfInitIntegralTimerSpace"

    END SUBROUTINE tlfInitIntegralTimerSpace

    FUNCTION tlfNewIntegralTimer(name, idTimerIn) result(idTimerOut)

        IMPLICIT NONE

        CHARACTER(len=*), INTENT(in), OPTIONAL :: name
        INTEGER,          INTENT(in), OPTIONAL :: idTimerIn
        INTEGER                                :: idTimerOut

        idTimerOut = -1
        IF(PRESENT(idTimerIn)) THEN
            IF(integralTimerSpace(idTimerIn)%stat .EQV. .FALSE.) THEN
                idTimerOut = idTimerIn
            ELSE                
                WRITE(*, '(A,I0,A)') "ERROR => tlfNewIntegralTimer: Integral timer with id = ", idTimerIn, " already exists."
                ERROR STOP
            END IF
        END IF

        IF(idTimerOut == -1) THEN

            IF(numActiveIntegralTimers == integralTimerSpaceSize) THEN
                ERROR STOP "ERROR => tlfNewIntegralTimer: The limit for the maximum number of integral timers was exceeded."
            END IF

            DO idTimerOut = 1, integralTimerSpaceSize
                IF(integralTimerSpace(idTimerOut)%stat .EQV. .FALSE.) THEN
                    EXIT
                END IF
            END DO

            IF(idTimerOut == integralTimerSpaceSize) THEN
                ERROR STOP "ERROR => tlfNewIntegralTimer: Unable to assign an id to the new integral timer."
            END IF

        END IF

        integralTimerSpace(idTimerOut)%name = ""
        IF(PRESENT(name)) THEN            
            IF(len_trim(name) > 0) THEN
                integralTimerSpace(idTimerOut)%name = name(1:min(128, len_trim(name)))
            END IF
        ENDIF

        integralTimerSpace(idTimerOut)%idTimer = idTimerOut
        integralTimerSpace(idTimerOut)%stat    = .TRUE.        

        numActiveIntegralTimers = numActiveIntegralTimers + 1

        WRITE(*,'(A,I0,A,A,A)') 'tlfNewIntegralTimer: Created new integral timer with id=', idTimerOut, &
        & ', name="', trim(integralTimerSpace(idTimerOut)%name), '"'

    END FUNCTION tlfNewIntegralTimer

    SUBROUTINE tlfAttachCoreTimerToIntegral(idCoreTimer, idIntegralTimer)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idCoreTimer
        INTEGER, INTENT(in) :: idIntegralTimer
        INTEGER             :: iTimer

        IF(integralTimerSpace(idIntegralTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "ERROR => tlfAttachCoreTimerToIntegral: Integral timer with id=", idIntegralTimer, " does not exist."
            ERROR STOP
        END IF

        IF(timerSpaceHeaders(idCoreTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "ERROR => tlfAttachCoreTimerToIntegral: Timer with id=", idCoreTimer, " does not exist."
            ERROR STOP
        END IF

        DO iTimer = 1, integralTimerSpace(idIntegralTimer)%numMembers
            IF(integralTimerSpace(idIntegralTimer)%listOfSingleTimers(iTimer) == idCoreTimer) THEN
                WRITE(*,*) "ERROR => tlfAttachCoreTimerToIntegral: Timer with id=", idCoreTimer, &
                " is already attached to Integral Timer with id=", idIntegralTimer
                ERROR STOP
            END IF
        END DO

        integralTimerSpace(idIntegralTimer)%numMembers = integralTimerSpace(idIntegralTimer)%numMembers + 1
        integralTimerSpace(idIntegralTimer)%listOfSingleTimers(integralTimerSpace(idIntegralTimer)%numMembers) = idCoreTimer


        WRITE(*,'(A,I0,A,I0)') "tlfAttachCoreTimerToIntegral: Single Timer with id=", idCoreTimer, &
                " was attached to Integral Timer with id=", idIntegralTimer

    END SUBROUTINE tlfAttachCoreTimerToIntegral

    SUBROUTINE tlfPrintIntegralTimerMembers(idIntegralTimer)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idIntegralTimer
        INTEGER             :: iTimer, idCoreTimer

        IF(integralTimerSpace(idIntegralTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "ERROR => tlfPrintIntegralTimerMembers: Integral timer with id=", idIntegralTimer, " does not exist."
            ERROR STOP
        END IF

        WRITE(*,'(A)') "********** INTEGRAL TIMER **********"
        WRITE(*,'(A,A,A)') 'NAME: "', TRIM(integralTimerSpace(idIntegralTimer)%name), '"'
        
        IF(integralTimerSpace(idIntegralTimer)%numMembers == 0) THEN
            WRITE(*,'(A)') "No Single Timers are attached to the Integral Timer"
        END IF

        WRITE(*,'(A)') "CONTAINS"
        WRITE(*,'(A)') "  ID     NAME"
        DO iTimer = 1, integralTimerSpace(idIntegralTimer)%numMembers

            idCoreTimer = integralTimerSpace(idIntegralTimer)%listOfSingleTimers(iTimer)            
            WRITE(*,'(I4,5x,A,A,A)') idCoreTimer, '"', TRIM(timerSpaceHeaders(idCoreTimer)%name), '"'

        END DO

    END SUBROUTINE tlfPrintIntegralTimerMembers

    SUBROUTINE tlfUpdateIntegralTimer(idIntegralTimer)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idIntegralTimer
        INTEGER             :: iTimer, idCoreTimer

        IF(integralTimerSpace(idIntegralTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "ERROR => tlfUpdateIntegralTimer: Integral timer with id=", idIntegralTimer, " does not exist."
            ERROR STOP
        END IF

        integralTimerSpace(idIntegralTimer)%totT  = 0.0D0
        integralTimerSpace(idIntegralTimer)%lastT = 0.0D0

        DO iTimer = 1, integralTimerSpace(idIntegralTimer)%numMembers

            idCoreTimer = integralTimerSpace(idIntegralTimer)%listOfSingleTimers(iTimer)

            IF(timerSpaceCores(idCoreTimer)%stat .EQV. .TRUE.) THEN            
                WRITE(*, '(A,I0,A)') "ERROR => tlfUpdateIntegralTimer: Timer with id=", idCoreTimer, " is currently running."
                ERROR STOP
            END IF

            integralTimerSpace(idIntegralTimer)%totT  = integralTimerSpace(idIntegralTimer)%totT  + timerSpaceCores(idCoreTimer)%totT
            integralTimerSpace(idIntegralTimer)%lastT = integralTimerSpace(idIntegralTimer)%lastT + timerSpaceCores(idCoreTimer)%lastT

        END DO

        WRITE(*,'(A,I0,A,A,A)') "tlfUpdateIntegralTimer: Integral Timer with id=", idIntegralTimer, " updated"

    END SUBROUTINE tlfUpdateIntegralTimer

    FUNCTION tlfGetIntegralTimerFieldValueDP(idTimer, idField) result(fieldValue)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idTimer
        INTEGER, INTENT(in) :: idField
        DOUBLE PRECISION    :: fieldValue

        IF(integralTimerSpace(idTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "ERROR => tlfGetIntegralTimerFieldValueDP: Integral timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(idField == getIntegralTimerTotalTime) THEN
            fieldValue = integralTimerSpace(idTimer)%totT
            RETURN
        END IF

        WRITE(*, '(A,I0,A)') "ERROR => tlfGetIntegralTimerFieldValueDP: Field id=", idField, " is not supported."
        ERROR STOP

    END FUNCTION tlfGetIntegralTimerFieldValueDP

    FUNCTION tlfGetIntegralTimerFieldValueI(idTimer, idField) result(fieldValue)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idTimer
        INTEGER, INTENT(in) :: idField
        INTEGER             :: fieldValue

        IF(integralTimerSpace(idTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "ERROR => tlfGetIntegralTimerFieldValueI: Integral timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF

        IF(idField == getIntegralTimerNumPE) THEN
            fieldValue = integralTimerSpace(idTimer)%numPE
            RETURN
        END IF

        WRITE(*, '(A,I0,A)') "ERROR => tlfGetIntegralTimerFieldValueI: Field id=", idField, " is not supported."
        ERROR STOP

    END FUNCTION tlfGetIntegralTimerFieldValueI

    SUBROUTINE tlfPrintIntegralTimerStatus(idTimer, fmtIn)

        IMPLICIT NONE

        INTEGER, INTENT(in)           :: idTimer
        INTEGER, INTENT(in), OPTIONAL :: fmtIn

        INTEGER                       :: fmt


        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfPrintIntegralTimerStatus: Integral timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF        

        fmt = 0
        IF(PRESENT(fmtIn)) THEN
            IF(fmtIn == 1) fmt = 1 ! Вывод только строки с параметрами
            IF(fmtIn == 2) fmt = 2 ! Вывод заголовка + строки с параметрами
        END IF

        IF(fmt == 0) THEN
            WRITE(*,'(a)') 'Integral Timer Information:'
            WRITE(*,'(a,i12)') '  ID            : ', integralTimerSpace(idTimer)%idTimer
            WRITE(*,'(a,a)')  '  Name          : ', trim(integralTimerSpace(idTimer)%name)            
            WRITE(*,'(a)')    '  Time (s):'            
            WRITE(*,'(a,G12.5)') '    Total       : ', integralTimerSpace(idTimer)%totT
            WRITE(*,'(a,G12.5)') '    Last        : ', integralTimerSpace(idTimer)%lastT
        END IF

        IF(fmt /= 0) THEN

            IF(fmt == 2) THEN
                WRITE(*,'(a)') &
                '  ID     Total(s)      Last(s) **** Name ****'
                WRITE(*,'(a)') &
                '---------------------------------------------------------------------------------------------'
            END IF

            WRITE(*, '(I4,1x,G12.5,1x,G12.5,1x,a,a,a)') &
            integralTimerSpace(idTimer)%idTimer, &
            integralTimerSpace(idTimer)%totT, integralTimerSpace(idTimer)%lastT, &
            '"', trim(integralTimerSpace(idTimer)%name), '"'
        END IF

    END SUBROUTINE tlfPrintIntegralTimerStatus

#ifdef _TLF_USE_MPI_
    SUBROUTINE tlfUpdateIntegralTimerMPI(idIntegralTimer, COMM, updateFlagIn)

        IMPLICIT NONE

        INTEGER, INTENT(in) :: idIntegralTimer
        INTEGER, INTENT(in) :: COMM
        LOGICAL, INTENT(in), OPTIONAL :: updateFlagIn
        LOGICAL                       :: updateFlag

        INTEGER             :: mpiRank, mpiSize, ierror
        INTEGER             :: iTimer, idCoreTimer

        updateFlag = .FALSE.
        IF(PRESENT(updateFlagIn)) THEN
            updateFlag = updateFlagIn
        END IF

        call MPI_COMM_SIZE(COMM, mpiSize, ierror)
        call MPI_COMM_RANK(COMM, mpiRank, ierror)

        IF(integralTimerSpace(idIntegralTimer)%stat .EQV. .FALSE.) THEN
            WRITE(*,*) "PE ", mpiRank, " ERROR => tlfUpdateIntegralTimerMPI: Integral timer with id=", idIntegralTimer, " does not exist."
            ERROR STOP
        END IF

        IF(updateFlag) THEN
            integralTimerSpace(idIntegralTimer)%totT  = 0.0D0
            integralTimerSpace(idIntegralTimer)%lastT = 0.0D0

            DO iTimer = 1, integralTimerSpace(idIntegralTimer)%numMembers

                idCoreTimer = integralTimerSpace(idIntegralTimer)%listOfSingleTimers(iTimer)

                IF(timerSpaceCores(idCoreTimer)%stat .EQV. .TRUE.) THEN            
                    WRITE(*, '(A,I5,A,I0,A)') "PE ", mpiRank, " ERROR => tlfUpdateIntegralTimerMPI: Timer with id=", idCoreTimer, " is currently running."
                    ERROR STOP
                END IF

                integralTimerSpace(idIntegralTimer)%totT  = integralTimerSpace(idIntegralTimer)%totT  + timerSpaceCores(idCoreTimer)%totT
                integralTimerSpace(idIntegralTimer)%lastT = integralTimerSpace(idIntegralTimer)%lastT + timerSpaceCores(idCoreTimer)%lastT

            END DO
        END IF

        integralTimerSpace(idIntegralTimer)%numPE = mpiSize

        CALL MPI_Allreduce(integralTimerSpace(idIntegralTimer)%totT, integralTimerSpace(idIntegralTimer)%mpiTotT, &
             & 1, MPI_DOUBLE_PRECISION, MPI_SUM, COMM, ierror)
        CALL MPI_Allreduce(integralTimerSpace(idIntegralTimer)%totT, integralTimerSpace(idIntegralTimer)%mpiMinTotT, &
             & 1, MPI_DOUBLE_PRECISION, MPI_MIN, COMM, ierror)
        CALL MPI_Allreduce(integralTimerSpace(idIntegralTimer)%totT, integralTimerSpace(idIntegralTimer)%mpiMaxTotT, &
             & 1, MPI_DOUBLE_PRECISION, MPI_MAX, COMM, ierror)
        integralTimerSpace(idIntegralTimer)%mpiAvgTotT  = integralTimerSpace(idIntegralTimer)%mpiTotT  / DBLE(mpiSize)
        
        CALL MPI_Allreduce(integralTimerSpace(idIntegralTimer)%lastT, integralTimerSpace(idIntegralTimer)%mpiLastT, &
             & 1, MPI_DOUBLE_PRECISION, MPI_SUM, COMM, ierror)
        CALL MPI_Allreduce(integralTimerSpace(idIntegralTimer)%lastT, integralTimerSpace(idIntegralTimer)%mpiMinLastT, &
             & 1, MPI_DOUBLE_PRECISION, MPI_MIN, COMM, ierror)
        CALL MPI_Allreduce(integralTimerSpace(idIntegralTimer)%lastT, integralTimerSpace(idIntegralTimer)%mpiMaxLastT, &
             & 1, MPI_DOUBLE_PRECISION, MPI_MAX, COMM, ierror)
        integralTimerSpace(idIntegralTimer)%mpiAvgLastT = integralTimerSpace(idIntegralTimer)%mpiLastT / DBLE(mpiSize)

        CALL MPI_BARRIER(COMM, ierror)
        IF(mpiRank == 0) THEN
            WRITE(*,'(A,I0,A,A,A)') "tlfUpdateIntegralTimerMPI: Integral Timer with id=", idIntegralTimer, " updated"
        END IF

    END SUBROUTINE tlfUpdateIntegralTimerMPI

    SUBROUTINE tlfPrintIntegralTimerStatusMPI(idTimer)

        IMPLICIT NONE

        INTEGER, INTENT(in)           :: idTimer

        IF(timerSpaceCores(idTimer)%idTimer == -1) THEN            
            WRITE(*, '(A,I0,A)') "ERROR => tlfPrintIntegralTimerStatusMPI: Integral timer with id=", idTimer, " does not exist."
            ERROR STOP
        END IF
        
        WRITE(*,'(A)') "********** INTEGRAL TIMER **********"
        WRITE(*,'(A,I0)')  'ID:             ', idTimer
        WRITE(*,'(A,A,A)') 'NAME:          "', TRIM(integralTimerSpace(idTimer)%name), '"'
        WRITE(*,'(A,I0)')  'MPI GROUP SIZE: ', integralTimerSpace(idTimer)%numPE
        WRITE(*,'(a)') &
            '                   Min            Max            Avg            Sum'
        WRITE(*, '(A,2x,G13.5,2x,G13.5,2x,G13.5,2x,G13.5)') &
            '  TOTAL', integralTimerSpace(idTimer)%mpiMinTotT, integralTimerSpace(idTimer)%mpiMaxTotT, &
            integralTimerSpace(idTimer)%mpiAvgTotT, integralTimerSpace(idTimer)%mpiTotT
        WRITE(*, '(A,2x,G13.5,2x,G13.5,2x,G13.5,2x,G13.5)') &
            '   LAST', integralTimerSpace(idTimer)%mpiMinLastT, integralTimerSpace(idTimer)%mpiMaxLastT, &
            integralTimerSpace(idTimer)%mpiAvgLastT, integralTimerSpace(idTimer)%mpiLastT        

    END SUBROUTINE tlfPrintIntegralTimerStatusMPI
#endif



END MODULE timerLibFortran

#undef _TLF_USE_MPI_

#if 0
INTEGER            :: idTimer

        LOGICAL            :: stat
        CHARACTER(len=128) :: name
        
        DOUBLE PRECISION   :: totT   
        DOUBLE PRECISION   :: lastT

        INTEGER            :: numMembers
        INTEGER            :: listOfSingleTimers(timerSpaceSize)

        INTEGER            :: numPE
        DOUBLE PRECISION   :: mpiTotT
        DOUBLE PRECISION   :: mpiMinTotT
        DOUBLE PRECISION   :: mpiMaxTotT
        DOUBLE PRECISION   :: mpiAvgTotT
        DOUBLE PRECISION   :: mpiLastT
        DOUBLE PRECISION   :: mpiMinLastT
        DOUBLE PRECISION   :: mpiMaxLastT
        DOUBLE PRECISION   :: mpiAvgLastT
#endif