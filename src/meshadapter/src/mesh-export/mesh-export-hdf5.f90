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
MODULE MOD_ExportToHDF5
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE ExportMeshToHDF5CODA
  MODULE PROCEDURE ExportMesh3DToHDF5CODA
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: ExportMeshToHDF5CODA
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
SUBROUTINE ExportMesh3DToHDF5CODA(&
  FileName,VarNames,BoundaryType,BCFacesToNodes,BCFacesToType,ElementsToNodes,NodesCoordinates,MasterSlavesToNodes)
!----------------------------------------------------------------------------------------------------------------------!
USE HDF5
USE MOD_HDF5_Tools
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)    :: FileName
CHARACTER(LEN=*),INTENT(IN)    :: VarNames(1:3)
INTEGER,INTENT(INOUT)          :: BoundaryType(:,:)
INTEGER,INTENT(INOUT)          :: BCFacesToNodes(:,:)
INTEGER,INTENT(INOUT)          :: BCFacesToType(:)
INTEGER,INTENT(INOUT)          :: ElementsToNodes(:,:)
REAL,INTENT(INOUT)             :: NodesCoordinates(:,:)
INTEGER,INTENT(INOUT),OPTIONAL :: MasterSlavesToNodes(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iBC
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: nBCFaces
INTEGER :: nQuad4Quad
CHARACTER(LEN=256) :: iBCStr
!----------------------------------------------------------------------------------------------------------------------!
INTEGER(HID_T)     :: file_id
INTEGER(HID_T)     :: mygroup_id
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: CODA_NumberOfCellTypes(1:1)
INTEGER            :: CODA_NumberOfCells(1:1)
INTEGER            :: CODA_NumberOfVariables(1:1)
INTEGER            :: CODA_SpansAllCells(1:1)
INTEGER            :: CODA_MappingCellType2Index(1:4)
INTEGER            :: CODA_Version(1:1)
CHARACTER(LEN=256) :: CODA_MeshImportFormat
CHARACTER(LEN=256) :: CODA_FullFileName
CHARACTER(LEN=256) :: CODA_FileExtension
CHARACTER(LEN=256) :: CODA_Variable0
CHARACTER(LEN=256) :: CODA_Variable1
CHARACTER(LEN=256) :: CODA_Variable2
CHARACTER(LEN=256) :: CODA_CellType
CHARACTER(LEN=256) :: CODA_EmptyString
CHARACTER(LEN=256) :: CODA_NotInitialized
CHARACTER(LEN=256),ALLOCATABLE :: CODA_CADGroupID(:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrL
CHARACTER(LEN=256) :: StrR
!----------------------------------------------------------------------------------------------------------------------!

nElems     = SIZE(ElementsToNodes,2)
nNodes     = SIZE(NodesCoordinates,2)
nBCFaces   = SIZE(BCFacesToNodes,2)

IF (PRESENT(MasterSlavesToNodes) .EQV. .TRUE.) THEN
  nQuad4Quad = SIZE(MasterSlavesToNodes,2)
END IF

ElementsToNodes  = ElementsToNodes-1
BCFacesToNodes   = BCFacesToNodes-1

IF (PRESENT(MasterSlavesToNodes) .EQV. .TRUE.) THEN
  MasterSlavesToNodes = MasterSlavesToNodes-1
END IF

! Information for CODA files
CODA_FileExtension    = ".h5"
CODA_FullFileName     = TRIM(FileName)//TRIM(CODA_FileExtension)
CODA_EmptyString      = "<no information>"
CODA_NotInitialized   = "<not initialized>"
CODA_MeshImportFormat = "HDF5"
CODA_Version          = 1

StrL = "Writing Mesh"
StrR = TRIM(CODA_FullFileName)
CALL PrintAnalyze(StrL,StrR)

CALL HDF5_OpenFile(file_id,CODA_FullFileName,status="NEW",action="WRITE")

CALL HDF5_CreateGroup(file_id,"FS:Mesh")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/CellType0")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0/DataSpecification")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1/DataSpecification")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2/DataSpecification")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/MappingCellType2Index")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Node")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4/CellAttributes")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Hexa8")
IF (PRESENT(MasterSlavesToNodes) .EQV. .TRUE.) THEN
  SELECT CASE(SIZE(MasterSlavesToNodes,1))
    CASE(6)
      CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad2Quad")
    CASE(9)
      CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4Quad")
  END SELECT
END IF
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/NamesOfCellAttributeValues")
CALL HDF5_CreateGroup(file_id,"FS:Mesh/UnstructuredCells/NamesOfCellAttributeValues/CADGroupID")

CALL HDF5_OpenGroup(file_id,"FS:Mesh",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","MeshFilename",CODA_FullFileName)
CALL HDF5_WriteAttribute(mygroup_id,"","MeshImportFormat",CODA_MeshImportFormat)
CALL HDF5_WriteAttribute(mygroup_id,"","Version",CODA_Version)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_NumberOfCellTypes = 1
CODA_NumberOfCells     = nNodes
CODA_NumberOfVariables = 3
CODA_SpansAllCells     = 1

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCellTypes",CODA_NumberOfCellTypes)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfVariables",CODA_NumberOfVariables)
CALL HDF5_WriteAttribute(mygroup_id,"","SpansAllCells",CODA_SpansAllCells)
CALL HDF5_WriteDataSet(mygroup_id,"Values",NodesCoordinates)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_CellType  = "Node"
CODA_Variable0 = VarNames(1)
CODA_Variable1 = VarNames(2)
CODA_Variable2 = VarNames(3)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/CellType0",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_CellType)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_Variable0)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable0/DataSpecification",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","InfoString",CODA_EmptyString)
CALL HDF5_WriteAttribute(mygroup_id,"","Type",CODA_NotInitialized)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_Variable1)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable1/DataSpecification",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","InfoString",CODA_EmptyString)
CALL HDF5_WriteAttribute(mygroup_id,"","Type",CODA_NotInitialized)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Name",CODA_Variable2)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Datasets/Coordinates/Variable2/DataSpecification",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","InfoString",CODA_EmptyString)
CALL HDF5_WriteAttribute(mygroup_id,"","Type",CODA_NotInitialized)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_MappingCellType2Index =(/0,1,2,3/)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/MappingCellType2Index",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","Node",(/CODA_MappingCellType2Index(1)/))
CALL HDF5_WriteAttribute(mygroup_id,"","Quad4",(/CODA_MappingCellType2Index(2)/))
CALL HDF5_WriteAttribute(mygroup_id,"","Hexa8",(/CODA_MappingCellType2Index(3)/))
IF (PRESENT(MasterSlavesToNodes) .EQV. .TRUE.) THEN
  SELECT CASE(SIZE(MasterSlavesToNodes,1))
    CASE(6)
      CALL HDF5_WriteAttribute(mygroup_id,"","Quad2Quad",(/CODA_MappingCellType2Index(4)/))
    CASE(9)
      CALL HDF5_WriteAttribute(mygroup_id,"","Quad4Quad",(/CODA_MappingCellType2Index(4)/))
  END SELECT
END IF
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
IF (ALLOCATED(CODA_CADGroupID)) THEN
  DEALLOCATE(CODA_CADGroupID)
END IF
ALLOCATE(CODA_CADGroupID(1:SIZE(BoundaryType,1)))

DO iBC=1,SIZE(BoundaryType,1)
  WRITE(CODA_CADGroupID(iBC),"(I0)") BoundaryType(iBC,1)
END DO

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/NamesOfCellAttributeValues/CADGroupID",mygroup_id)
DO iBC=1,SIZE(BoundaryType,1)
  WRITE(iBCStr,"(I0)") iBC
  CALL HDF5_WriteAttribute(mygroup_id,"",TRIM(iBCStr),CODA_CADGroupID(iBC))
END DO
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_NumberOfCells = nNodes

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Node",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_NumberOfCells = nBCFaces

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",BCFacesToNodes)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4/CellAttributes",mygroup_id)
CALL HDF5_WriteDataSet(mygroup_id,"CADGroupID",BCFacesToType)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
CODA_NumberOfCells = nElems

CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Hexa8",mygroup_id)
CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",ElementsToNodes)
CALL HDF5_CloseGroup(mygroup_id)

! Information for CODA files
IF (PRESENT(MasterSlavesToNodes) .EQV. .TRUE.) THEN
  CODA_NumberOfCells = nQuad4Quad
  SELECT CASE(SIZE(MasterSlavesToNodes,1))
    CASE(6)
      CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad2Quad",mygroup_id)
    CASE(9)
      CALL HDF5_OpenGroup(file_id,"FS:Mesh/UnstructuredCells/Quad4Quad",mygroup_id)
  END SELECT
  CALL HDF5_WriteAttribute(mygroup_id,"","NumberOfCells",CODA_NumberOfCells)
  CALL HDF5_WriteDataSet(mygroup_id,"Cell2Node",MasterSlavesToNodes)
  CALL HDF5_CloseGroup(mygroup_id)
END IF

CALL HDF5_CloseFile(file_id)

ElementsToNodes  = ElementsToNodes+1
BCFacesToNodes   = BCFacesToNodes+1

IF (PRESENT(MasterSlavesToNodes) .EQV. .TRUE.) THEN
  MasterSlavesToNodes = MasterSlavesToNodes+1
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ExportMesh3DToHDF5CODA
!======================================================================================================================!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_ExportToHDF5
!======================================================================================================================!
