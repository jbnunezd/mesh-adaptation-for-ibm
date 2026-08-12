!======================================================================================================================!
#include "main.h"
!======================================================================================================================!
!
!======================================================================================================================!
PROGRAM MESH_ADAPTATION_FOR_IBM
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools,ONLY: IgnoredStrings
USE MOD_ConfigFilesTools,ONLY: ReadParameterFile
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshAdaptation,ONLY: InitializeMeshAdaptation
USE MOD_MeshAdaptation,ONLY: MeshAdaptation
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: Time
CHARACTER(LEN=255) :: Header
CHARACTER(LEN=255) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!

ElapsedTime = ""
StartTime   = RunningTime()

CALL InitializeMain()

Header = "INITIALIZING "//TRIM(ProgramName)
CALL PrintHeader(Header)
CALL PrintCurrentTime()
CALL ReadParameterFile()
CALL InitializeMeshAdaptation()
CALL IgnoredStrings()

Time = RunningTime()
CALL ComputeRuntime(Time-StartTime,ElapsedTime)
ElapsedTime = "[ "//ADJUSTR(TRIM(ElapsedTime))//" ]"
Header = "INITIALIZATION DONE! "//TRIM(ElapsedTime)
CALL PrintHeader(Header)

CALL MeshAdaptation()

SWRITE(UNIT_SCREEN,*)

Time = RunningTime()
CALL ComputeRuntime(Time-StartTime,ElapsedTime)
ElapsedTime = "[ "//ADJUSTR(TRIM(ElapsedTime))//" ]"
Header = TRIM(ProgramName)//" FINISHED! "//TRIM(ElapsedTime)
CALL PrintHeader(Header)

!----------------------------------------------------------------------------------------------------------------------!
END PROGRAM MESH_ADAPTATION_FOR_IBM
!======================================================================================================================!
