!======================================================================================================================!
!
! IBM-MESH-ADAPTATION-TOOL
!
! Copyright (c) 2020 by Jonatan Nunez
!
! This program is free software: you can redistribute it and/or modify it under the terms of the GNU 
! General Public License as published by the Free Software Foundation, either version 3 of the License, 
! or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even 
! the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with this program.
! If not, see <https://www.gnu.org/licenses/>.
!
!======================================================================================================================!
!
!======================================================================================================================!
#include "main.h"
!======================================================================================================================!
!
!======================================================================================================================!
MODULE MOD_MeshAdapter
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshAdapter
  MODULE PROCEDURE InitializeMeshAdapter
END INTERFACE

INTERFACE MeshAdapter
  MODULE PROCEDURE MeshAdapter
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshAdapter
PUBLIC :: MeshAdapter
!----------------------------------------------------------------------------------------------------------------------!
!
!
!
!======================================================================================================================!
CONTAINS
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeMeshAdapter()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_nDims
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshAdapter_vars,ONLY: ParametersMeshAdapter
USE MOD_MeshAdapter_vars,ONLY: InitializeMeshAdapterIsDone
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport,      ONLY: InitializeMeshExport
USE MOD_MeshBuiltIn,     ONLY: InitializeMeshBuiltIn
USE MOD_GeometryImport,  ONLY: InitializeGeometryImport
USE MOD_MeshRefinement,  ONLY: InitializeMeshRefinement
USE MOD_MeshElementsList,ONLY: InitializeMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: ParametersMeshGeometry
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshAdapterIsDone .EQV. .TRUE.) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshAdapter not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESH-ADAPTER MODULE..."
CALL PrintHeader(Header)

ParametersMeshAdapter%MeshDimension               = GetInteger('MeshDimension','3')
ParametersMeshAdapter%ProjectName                 = GetString('ProjectName')
ParametersMeshAdapter%WhichMeshConstructionMethod = GetString('WhichMeshConstructionMethod')
ParametersMeshAdapter%ProgramName                 = ProgramName
ParametersMeshAdapter%FileVersion                 = "2024.09"

PP_nDims = ParametersMeshAdapter%MeshDimension

SELECT CASE(LowerCase(ParametersMeshAdapter%WhichMeshConstructionMethod))
  CASE('mesh-builtin')
    CALL InitializeMeshBuiltIn()
  CASE DEFAULT
  ErrorMessage = "InitializeMeshAdapter: Unknown Mesh Construction Method"
  CALL PrintError(__STAMP__,ErrorMessage)  
END SELECT

CALL InitializeMeshExport()
CALL InitializeMeshElementsList()
CALL InitializeMeshRefinement()

SELECT CASE(LowerCase(ParametersMeshRefinement%WhichMeshRefinement))
  CASE("refine-elements-around-geometry")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("imported-geometry")
        CALL InitializeGeometryImport()
    END SELECT
  CASE("refine-elements-around-geometry-and-inside-box")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("imported-geometry")
        CALL InitializeGeometryImport()
    END SELECT
END SELECT

InitializeMeshAdapterIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshAdapter
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshAdapter()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CountElemsByLevel
USE MOD_MeshMainMethods,ONLY: CountNodes
USE MOD_MeshMainMethods,ONLY: PrintElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList,ONLY: SetUniqueNodes
USE MOD_MeshElementsList,ONLY: SetUniqueElemID
USE MOD_MeshElementsList,ONLY: SetUniqueSideID
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport, ONLY: MeshExport
USE MOD_MeshBuiltIn,ONLY: MeshBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryImport,ONLY: GeometryImport
USE MOD_GeometryImport,ONLY: ExtractPointsFromGeometryImport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList,          ONLY: CreateElementsList
USE MOD_MeshAdapter_vars,          ONLY: ParametersMeshAdapter
USE MOD_MeshRefinement_vars,       ONLY: MaxRefinementLevel
USE MOD_MeshRefinement_vars,       ONLY: RefineMeshAroundGeometry
USE MOD_MeshRefinement_vars,       ONLY: FlagElementsForRefinement
USE MOD_MeshRefinement_vars,       ONLY: ParametersMeshRefinement
USE MOD_MeshRefinementFlagging,    ONLY: FlagElementsForBalancing
USE MOD_MeshRefinementFlagging,    ONLY: InitializeElementsToRefineArray
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometryDistribution,ONLY: DistributeSTLFacetsInElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinementSplitting,   ONLY: RefineElements
USE MOD_MeshRefinementConnectivity,ONLY: CreateNonConformingConnectivity
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExportElementsList,ONLY: CreateMeshDataArrays
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: ParametersMeshGeometry
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLevel
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: BaseFileName
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: nNodes
INTEGER             :: nElems
INTEGER,ALLOCATABLE :: nElemsByLevel(:)
INTEGER             :: MeshInfoData(1:3)
REAL                :: CalcTimeIni
REAL                :: CalcTimeEnd
CHARACTER(LEN=256)  :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: MeshInfoVarNames(1:3)
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=:),ALLOCATABLE :: StrLine1
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: AlreadyDistributed
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateSeparatingLine("-",StrLine1)

SELECT CASE(LowerCase(ParametersMeshAdapter%WhichMeshConstructionMethod))
  CASE('mesh-builtin')
    CALL MeshBuiltIn()
  CASE DEFAULT
  ErrorMessage = "MeshAdapter: Unknown Mesh Construction Method"
  CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

SELECT CASE(LowerCase(ParametersMeshRefinement%WhichMeshRefinement))
  CASE("refine-elements-around-geometry")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("imported-geometry")
        CALL GeometryImport()
        CALL ExtractPointsFromGeometryImport()
    END SELECT
  CASE("refine-elements-around-geometry-and-inside-box")
    SELECT CASE(LowerCase(ParametersMeshGeometry%WhichBodyGeometry))
      CASE("imported-geometry")
        CALL GeometryImport()
        CALL ExtractPointsFromGeometryImport()
    END SELECT
END SELECT

BaseFileName = TRIM(MeshInfo%BaseFileName)

AlreadyDistributed = .FALSE.

DO iLevel=0,MaxRefinementLevel
  WRITE(MeshInfo%BaseFileName,"(A,A,I0.2)") TRIM(BaseFileName), "_L", iLevel

  ! No mesh refinement at this level
  IF (iLevel .EQ. 0) THEN
    SWRITE(UNIT_SCREEN,*)
    Header = "CREATING MESH..."
    CALL PrintMessage(Header)

    CalcTimeIni = RunningTime()

    CALL CreateElementsList()

    ! Compute nElems and nNodes
    CALL CountElems(ElemList,nElems)
    CALL CountNodes(ElemList,nNodes)

    MeshInfoData(1) = iLevel
    MeshInfoData(2) = nElems
    MeshInfoData(3) = nNodes
    MeshInfoVarNames(1) = "Level"
    MeshInfoVarNames(2) = "nElems"
    MeshInfoVarNames(3) = "nNodes"
    CALL PrintArrayInfo("Mesh Information",MeshInfoVarNames,MeshInfoData)
    
    CALL CreateMeshDataArrays()
    CALL MeshExport()

    CalcTimeEnd = RunningTime()

    CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)

    Header = "Elapsed Time"
    CALL PrintAnalyze(Header,ElapsedTime)

    IF (ParametersMeshRefinement%nRefinedBox .EQ. 0) THEN
      IF (RefineMeshAroundGeometry .EQV. .TRUE.) THEN
        CALL DistributeSTLFacetsInElemList()
      END IF
    END IF
  END IF

  ! Mesh refinement
  IF (iLevel .GT. 0) THEN

    SWRITE(UNIT_SCREEN,*)
    Header = "REFINING MESH..."
    CALL PrintMessage(Header)

    CalcTimeIni = RunningTime()

    CALL InitializeElementsToRefineArray()
    CALL FlagElementsForRefinement(iLevel)
    CALL FlagElementsForBalancing(iLevel)
    CALL RefineElements(iLevel)
    CALL CreateMeshDataArrays()
    CALL CreateNonConformingConnectivity()

    ! Compute nElems and nNodes
    CALL CountElems(ElemList,nElems)
    CALL CountNodes(ElemList,nNodes)
    CALL CountElemsByLevel(ElemList,iLevel,nElemsByLevel)

    MeshInfoData(1) = iLevel
    MeshInfoData(2) = nElems
    MeshInfoData(3) = nNodes
    MeshInfoVarNames(1) = "Level"
    MeshInfoVarNames(2) = "nElems"
    MeshInfoVarNames(3) = "nNodes"
    CALL PrintArrayInfo("Mesh Information",MeshInfoVarNames,MeshInfoData)

    CalcTimeEnd = RunningTime()
    CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
    Header = "Elapsed Time"
    CALL PrintAnalyze(Header,ElapsedTime)

    CalcTimeIni = RunningTime()
    !************************************************************!
    IF (ParametersMeshRefinement%nRefinedBox .GT. 0) THEN
      IF (iLevel .GE. ParametersMeshRefinement%MaxBoxRefinementLevel(1)) THEN
        IF ((RefineMeshAroundGeometry .EQV. .TRUE.) .AND. (AlreadyDistributed .EQV. .FALSE.)) THEN
          ! Compute nElems and nNodes
          CALL CountElems(ElemList,nElems)
          CALL CountNodes(ElemList,nNodes)
          CALL CountElemsByLevel(ElemList,iLevel,nElemsByLevel)

          MeshInfoData(1) = iLevel
          MeshInfoData(2) = nElems
          MeshInfoData(3) = nNodes
          MeshInfoVarNames(1) = "Level"
          MeshInfoVarNames(2) = "nElems"
          MeshInfoVarNames(3) = "nNodes"
          CALL PrintArrayInfo("Mesh Information",MeshInfoVarNames,MeshInfoData)
        ELSEIF ((RefineMeshAroundGeometry .EQV. .TRUE.) .AND. (AlreadyDistributed .EQV. .TRUE.)) THEN

          CALL CountElems(ElemList,nElems)
          CALL CountNodes(ElemList,nNodes)
          CALL CountElemsByLevel(ElemList,iLevel,nElemsByLevel)

          MeshInfoData(1) = iLevel
          MeshInfoData(2) = nElems
          MeshInfoData(3) = nNodes
          MeshInfoVarNames(1) = "Level"
          MeshInfoVarNames(2) = "nElems"
          MeshInfoVarNames(3) = "nNodes"
          CALL PrintArrayInfo("Mesh Information",MeshInfoVarNames,MeshInfoData)
        END IF
      END IF
    END IF
    !************************************************************!
    IF (ParametersMeshRefinement%nRefinedBox .GT. 0) THEN
      IF (iLevel .GE. ParametersMeshRefinement%MaxBoxRefinementLevel(1)) THEN
        IF ((RefineMeshAroundGeometry .EQV. .TRUE.) .AND. (AlreadyDistributed .EQV. .FALSE.)) THEN
          CALL DistributeSTLFacetsInElemList()
          AlreadyDistributed = .TRUE.
        END IF
      END IF
    END IF
    !************************************************************!

    CALL MeshExport()
    
    CalcTimeEnd = RunningTime()    
    CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
    Header = "Elapsed Time"
    CALL PrintAnalyze(Header,ElapsedTime)

  END IF
END DO

SWRITE(UNIT_SCREEN,*)
Header = "MESH STATISTICS"
CALL PrintMessage(Header)
DO iLevel=0,MaxRefinementLevel
  MeshInfoData(1) = iLevel
  MeshInfoData(2) = nElemsByLevel(iLevel)
  MeshInfoVarNames(1) = "Level"
  MeshInfoVarNames(2) = "nElems"
  CALL PrintArrayInfo("nElemsByLevel",MeshInfoVarNames(1:2),MeshInfoData(1:2))
END DO
SWRITE(UNIT_SCREEN,*)
      
!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshAdapter
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshAdapter
!======================================================================================================================!
