!======================================================================================================================!
#include "main.h"
!======================================================================================================================!
!
!======================================================================================================================!
PROGRAM MESHADAPTER
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools,ONLY: IgnoredStrings
USE MOD_ConfigFilesTools,ONLY: ReadParameterFile
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshAdapter,ONLY: InitializeMeshAdapter
USE MOD_MeshAdapter,ONLY: MeshAdapter
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: Time
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!

ElapsedTime = ""
StartTime   = RunningTime()

CALL InitializeMain()

Header = "INITIALIZING "//TRIM(ProgramName)
CALL PrintHeader(Header)
CALL PrintCurrentTime()
CALL ReadParameterFile()
CALL InitializeMeshAdapter()
CALL IgnoredStrings()

Time = RunningTime()
CALL ComputeRuntime(Time-StartTime,ElapsedTime)
ElapsedTime = "[ "//ADJUSTR(TRIM(ElapsedTime))//" ]"
Header = "INITIALIZATION DONE! "//TRIM(ElapsedTime)
CALL PrintHeader(Header)

CALL MeshAdapter()

SWRITE(UNIT_SCREEN,*)

Time = RunningTime()
CALL ComputeRuntime(Time-StartTime,ElapsedTime)
ElapsedTime = "[ "//ADJUSTR(TRIM(ElapsedTime))//" ]"
Header = TRIM(ProgramName)//" FINISHED! "//TRIM(ElapsedTime)
CALL PrintHeader(Header)

!----------------------------------------------------------------------------------------------------------------------!
END PROGRAM MESHADAPTER
!======================================================================================================================!
