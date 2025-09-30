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
MODULE MOD_MeshExport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshExport
  MODULE PROCEDURE InitializeMeshExport
END INTERFACE

INTERFACE MeshExport
  MODULE PROCEDURE MeshExport
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshExport
PUBLIC :: MeshExport
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
SUBROUTINE InitializeMeshExport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: SortArray
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport_vars,ONLY: MeshExportNames
USE MOD_MeshExport_vars,ONLY: MeshExportFormat
USE MOD_MeshExport_vars,ONLY: MeshExportFormatIndex
USE MOD_MeshExport_vars,ONLY: ParametersMeshExport
USE MOD_MeshExport_vars,ONLY: InitializeMeshExportIsDone
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: iVar
INTEGER             :: jVar
INTEGER             :: nVars
INTEGER             :: iFormat
INTEGER             :: temp
INTEGER             :: nMeshExportFormat
INTEGER             :: nMeshExportFormatIn
INTEGER,ALLOCATABLE :: MeshExportFormatIndexTemp(:)
INTEGER,ALLOCATABLE :: index_vector(:)
LOGICAL,ALLOCATABLE :: Mask(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshExportIsDone .EQV. .TRUE.) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshExport not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESHEXPORT MODULE..."
CALL PrintHeader(Header)

ParametersMeshExport%ExportMesh        = GetLogical('ExportMesh','.FALSE.')
ParametersMeshExport%ExportMeshAndData = GetLogical('ExportMeshAndData','.FALSE.')

nVars = 3
ALLOCATE(MeshExportNames(1:nVars))
MeshExportNames(1) = "tecplot-ascii"
MeshExportNames(2) = "hdf5-coda"

IF (ParametersMeshExport%ExportMesh .EQV. .TRUE.) THEN
  nMeshExportFormatIn = CountStrings('MeshExportFormat',0)
  IF ((nMeshExportFormatIn .GT. 0)) THEN
    IF (.NOT. ALLOCATED(MeshExportFormat)) THEN
      ALLOCATE(MeshExportFormat(1:nMeshExportFormatIn))
    END IF
    DO iVar=1,nMeshExportFormatIn
      MeshExportFormat(iVar) = GetString('MeshExportFormat')
    END DO
    IF (.NOT. ALLOCATED(MeshExportFormatIndex)) THEN
      ALLOCATE(MeshExportFormatIndex(1:nMeshExportFormatIn))
    END IF

    jVar = 0
    MeshExportFormatIndex = 0
    DO iVar=1,nMeshExportFormatIn
      DO iFormat=1,nVars
        IF (TRIM(MeshExportFormat(iVar)) .EQ. TRIM(MeshExportNames(iFormat))) THEN
          jVar = jVar + 1
          MeshExportFormatIndex(jVar) = iFormat
        END IF
      END DO
    END DO

    IF (nMeshExportFormatIn .EQ. 1) THEN
      IF (MeshExportFormatIndex(1) .EQ. 0) THEN
        nMeshExportFormat = 0
      ELSE
        nMeshExportFormat = 1
      END IF
    END IF

    IF (nMeshExportFormatIn .GT. 1) THEN
      nMeshExportFormat = 0
      ALLOCATE(Mask(1:nMeshExportFormatIn))
      Mask = .TRUE.
      DO iVar=nMeshExportFormatIn,2,-1
        Mask(iVar) = .NOT.(ANY(MeshExportFormatIndex(1:iVar-1) .EQ. MeshExportFormatIndex(iVar)))
      END DO
      DO iVar=1,nMeshExportFormatIn
        IF (MeshExportFormatIndex(iVar) .EQ. 0) THEN
          Mask(iVar) = .FALSE.
        END IF
      END DO
      temp = SIZE(PACK([(iVar,iVar=1,nMeshExportFormatIn)],Mask))
      ALLOCATE(index_vector(temp))
      index_vector = PACK([(iVar,iVar=1,nMeshExportFormatIn)],Mask)
      temp = SIZE(MeshExportFormatIndex(index_vector))
      ALLOCATE(MeshExportFormatIndexTemp(temp))
      MeshExportFormatIndexTemp = MeshExportFormatIndex(index_vector)
      nMeshExportFormat = SIZE(MeshExportFormatIndexTemp)
      DO iVar=1,SIZE(MeshExportFormatIndexTemp)
        IF (MeshExportFormatIndexTemp(iVar) .EQ. 0) THEN
          nMeshExportFormat = nMeshExportFormat - 1
        END IF
      END DO
    END IF

    IF (nMeshExportFormat .EQ. 0) THEN
      nMeshExportFormat = nVars
      DEALLOCATE(MeshExportFormatIndex)
      ALLOCATE(MeshExportFormatIndex(nMeshExportFormat))
      MeshExportFormatIndex = 0
      DO iVar=1,nMeshExportFormat
        MeshExportFormatIndex(iVar) = iVar
      END DO
    ELSEIF ((nMeshExportFormat .GT. 0) .AND. (nMeshExportFormat .LT. nMeshExportFormatIn)) THEN
      DEALLOCATE(MeshExportFormatIndex)
      ALLOCATE(MeshExportFormatIndex(nMeshExportFormat))
      MeshExportFormatIndex = MeshExportFormatIndexTemp
      IF (nMeshExportFormat .GT. 1) THEN
        CALL SortArray(MeshExportFormatIndex)
      END IF
    ELSEIF ((nMeshExportFormat .GT. 0) .AND. (nMeshExportFormat .EQ. nMeshExportFormatIn)) THEN
      IF (nMeshExportFormat .GT. 1) THEN
        CALL SortArray(MeshExportFormatIndex)
      END IF
    END IF
  ELSE
    IF (.NOT. ALLOCATED(MeshExportFormatIndex)) THEN
      ALLOCATE(MeshExportFormatIndex(nMeshExportFormat))
    END IF
    MeshExportFormatIndex = 0
    DO iVar=1,nMeshExportFormat
      MeshExportFormatIndex(iVar) = iVar
    END DO
  END IF
END IF

InitializeMeshExportIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshExport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshExport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_nDims
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport_vars,ONLY: ParametersMeshExport
USE MOD_MeshExport_vars,ONLY: MeshExportNames
USE MOD_MeshExport_vars,ONLY: MeshExportFormatIndex
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iFormat
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (ParametersMeshExport%ExportMesh .EQV. .FALSE.) THEN
  RETURN
END IF

DO iFormat=1,SIZE(MeshExportFormatIndex)
  SELECT CASE(PP_nDims)
    CASE(3)
      CALL MeshExport3D(OutputFormat=MeshExportNames(MeshExportFormatIndex(iFormat)))
    CASE DEFAULT
      ErrorMessage = "Invalid value for MeshDimension"
      CALL PrintError(__STAMP__,ErrorMessage)
  END SELECT
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshExport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshExport3D(OutputFormat)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryType
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel3D
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshExport_vars,ONLY: ParametersMeshExport
USE MOD_ExportToHDF5,   ONLY: ExportMeshToHDF5CODA
USE MOD_ExportToTECPLOT,ONLY: ExportMeshToTECPLOT_ASCII
USE MOD_ExportToTECPLOT,ONLY: ExportMeshAndDataToTECPLOT_ASCII
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: OutputFormat
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(LowerCase(TRIM(OutputFormat)))
  CASE('tecplot-ascii')
    IF (ParametersMeshExport%ExportMeshAndData .EQV. .TRUE.) THEN
      CALL ExportMeshAndDataToTECPLOT_ASCII(&
        FileName     = MeshInfo%BaseFileName,&
        ProjectName  = MeshInfo%ProjectName,&
        ProgramName  = MeshInfo%ProgramName,&
        FileVersion  = MeshInfo%FileVersion,&
        nDims        = MeshInfo%nDims,&
        NGeo         = MeshInfo%NGeo,&
        nElems       = MeshInfo%nElems,&
        nOutVars     = MeshInfo%nOutVars,&
        OutputTime   = REAL(MeshInfo%MaxRefLevel),&
        CoordNames   = MeshInfo%CoordNames3D,&
        VarNames     = MeshInfo%DataNames3D,&
        MeshData     = MeshData_ElementsToNodesCoordinates3D,&
        OutputData   = MeshData_ElementsToLevel3D)
    ELSE
      CALL ExportMeshToTECPLOT_ASCII(&
        FileName     = MeshInfo%BaseFileName,&
        ProjectName  = MeshInfo%ProjectName,&
        ProgramName  = MeshInfo%ProgramName,&
        FileVersion  = MeshInfo%FileVersion,&
        nDims        = MeshInfo%nDims,&
        NGeo         = MeshInfo%NGeo,&
        nElems       = MeshInfo%nElems,&
        OutputTime   = REAL(MeshInfo%MaxRefLevel),&
        CoordNames   = MeshInfo%CoordNames3D,&
        MeshData     = MeshData_ElementsToNodesCoordinates3D)
    END IF
  CASE('hdf5-coda')
    IF (ALLOCATED(MeshData_MasterSlavesToNodes) .EQV. .TRUE.) THEN
      CALL ExportMeshToHDF5CODA(&
      FileName            = MeshInfo%BaseFileName,&
      VarNames            = MeshInfo%CoordNames3D,&
      BoundaryType        = MeshData_BoundaryType,&
      BCFacesToNodes      = MeshData_BCFacesToNodes,&
      BCFacesToType       = MeshData_BCFacesToType,&
      ElementsToNodes     = MeshData_ElementsToNodes,&
      NodesCoordinates    = MeshData_NodesCoordinates,&
      MasterSlavesToNodes = MeshData_MasterSlavesToNodes)
    ELSE
      CALL ExportMeshToHDF5CODA(&
      FileName            = MeshInfo%BaseFileName,&
      VarNames            = MeshInfo%CoordNames3D,&
      BoundaryType        = MeshData_BoundaryType,&
      BCFacesToNodes      = MeshData_BCFacesToNodes,&
      BCFacesToType       = MeshData_BCFacesToType,&
      ElementsToNodes     = MeshData_ElementsToNodes,&
      NodesCoordinates    = MeshData_NodesCoordinates)
    END IF
  CASE DEFAULT
  ErrorMessage = "Unknown output format"
  CALL PrintError(__STAMP__,ErrorMessage)  
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshExport3D
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshExport
!======================================================================================================================!
