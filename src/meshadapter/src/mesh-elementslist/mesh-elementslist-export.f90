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
MODULE MOD_MeshExportElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateMeshDataArrays
  MODULE PROCEDURE CreateMeshDataArrays
END INTERFACE

INTERFACE CountElemsNodesBCFaces
  MODULE PROCEDURE CountElemsNodesBCFaces
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: CreateMeshDataArrays
PUBLIC :: CountElemsNodesBCFaces
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
SUBROUTINE CreateMeshDataArrays()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateMeshDataArrays3D()

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateMeshDataArrays
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateMeshDataArrays3D()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

! Count Elements, Nodes, and BCFaces
CALL CountElemsNodesBCFaces()

! Save Nodes-Coordinates in Array
CALL CreateArray3D_NodesCoordinates()

! Save Elements-Nodes-Coordinates in Array
CALL CreateArray3D_ElementsToNodesCoordinates()

! Save Elements-to-Nodes in Array
CALL CreateArray3D_ElementsToNodes()

! Save Elements-Level in Array
CALL CreateArray3D_ElementsToLevel()

! Save BCFaces-to-Nodes in Array
CALL CreateArray3D_BCFacesToNodes()

! Save Faces-to-Nodes in Array
CALL CreateArray3D_FacesToNodes()

! Save Elements-to-Faces in Array
CALL CreateArray3D_ElementsToFaces()

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateMeshDataArrays3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountElemsNodesBCFaces()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountNodes
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CountBCFaces
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems
INTEGER :: nNodes
INTEGER :: nBCFaces
!----------------------------------------------------------------------------------------------------------------------!

! Count Elements
CALL CountElems(ElemList,nElems)

! Count Nodes
CALL CountNodes(ElemList,nNodes)

! Count BCFaces
CALL CountBCFaces(ElemList,nBCFaces)

MeshInfo%nElems = nElems
MeshInfo%nNodes = nNodes

MeshArraysInfo%nElems   = nElems
MeshArraysInfo%nNodes   = nNodes
MeshArraysInfo%nBCFaces = nBCFaces

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing MeshInfo"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nElems   = ", nElems
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nNodes   = ", nNodes
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nBCFaces = ", nBCFaces
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountElemsNodesBCFaces
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_NodesCoordinates()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_NodesCoordinates
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: nNodes
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nNodes = MeshArraysInfo%nNodes

IF (ALLOCATED(MeshData_NodesCoordinates)) THEN
  DEALLOCATE(MeshData_NodesCoordinates)
END IF
ALLOCATE(MeshData_NodesCoordinates(1:3,1:nNodes))

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  DO iNode=1,aElem%nNodes
    MeshData_NodesCoordinates(1:3,aElem%Nodes(iNode)%Node%NodeID) = aElem%Nodes(iNode)%Node%Coords(1:3)
  END DO
  aElem => aElem%NextElem
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Nodes-Coordinates"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(1(2X,A8),1(2X,A))") "iNode", "Coordinates"
  FormatString = "(1(2X,I8),3(2X,SP,ES13.6E2))"
  DO iNode=1,nNodes
    WRITE(UNIT_SCREEN,FormatString) iNode, MeshData_NodesCoordinates(1:3,iNode)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_NodesCoordinates
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_ElementsToNodesCoordinates()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: BuildHexaMap
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j, k
INTEGER :: iNode
INTEGER :: iElem
INTEGER :: ElemID
INTEGER :: nNodes
INTEGER :: nElems
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: HexaMap(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nElems = MeshArraysInfo%nElems

IF (ALLOCATED(MeshData_ElementsToNodesCoordinates3D)) THEN
  DEALLOCATE(MeshData_ElementsToNodesCoordinates3D)
END IF
ALLOCATE(MeshData_ElementsToNodesCoordinates3D(1:3,0:PP_NGeo,0:PP_NGeo,0:PP_NGeo,1:nElems))

CALL BuildHexaMap(PP_NGeo,nNodes,HexaMap)

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  DO iNode=1,aElem%nNodes
    i = HexaMap(iNode,1)
    j = HexaMap(iNode,2)
    k = HexaMap(iNode,3)
    MeshData_ElementsToNodesCoordinates3D(1:3,i,j,k,ElemID) = aElem%Nodes(iNode)%Node%Coords(1:3)
  END DO
  aElem => aElem%NextElem
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Nodes-Coordinates"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8),1(2X,A))") "iElem", "iNode", "Coordinates"
  FormatString = "(2(2X,I8),3(2X,SP,ES13.6E2))"
  DO iElem=1,nElems
    DO iNode=1,nNodes
      i = HexaMap(iNode,1)
      j = HexaMap(iNode,2)
      k = HexaMap(iNode,3)
      WRITE(UNIT_SCREEN,FormatString) iElem, iNode, MeshData_ElementsToNodesCoordinates3D(1:3,i,j,k,iElem)
    END DO
    WRITE(UNIT_SCREEN,*)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_ElementsToNodesCoordinates
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_ElementsToNodes()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iElem
INTEGER :: nElems
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nElems = MeshArraysInfo%nElems

IF (ALLOCATED(MeshData_ElementsToNodes)) THEN
  DEALLOCATE(MeshData_ElementsToNodes)
END IF
ALLOCATE(MeshData_ElementsToNodes(1:8,1:nElems))

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  DO iNode=1,aElem%nNodes
    MeshData_ElementsToNodes(iNode,ElemID) = aElem%Nodes(iNode)%Node%NodeID
  END DO
  aElem => aElem%NextElem
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "iElem", "NodeIDs"
  FormatString = "(1(2X,I8),8(2X,I8))"
  DO iElem=1,nElems
    WRITE(UNIT_SCREEN,FormatString) iElem, MeshData_ElementsToNodes(1:8,iElem)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_ElementsToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_ElementsToLevel()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: nElems
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nElems = MeshArraysInfo%nElems

IF (ALLOCATED(MeshData_ElementsToLevel)) THEN
  DEALLOCATE(MeshData_ElementsToLevel)
END IF
ALLOCATE(MeshData_ElementsToLevel(1:nElems))

IF (ALLOCATED(MeshData_ElementsToLevel3D)) THEN
  DEALLOCATE(MeshData_ElementsToLevel3D)
END IF
ALLOCATE(MeshData_ElementsToLevel3D(1:2,0:PP_NGeo,0:PP_NGeo,0:PP_NGeo,1:nElems))

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  MeshData_ElementsToLevel(ElemID) = aElem%Level
  MeshData_ElementsToLevel3D(1,0:PP_NGeo,0:PP_NGeo,0:PP_NGeo,ElemID) = REAL(aElem%Level)
  MeshData_ElementsToLevel3D(2,0:PP_NGeo,0:PP_NGeo,0:PP_NGeo,ElemID) = REAL(aElem%Flag)
  aElem => aElem%NextElem
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Level"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "iElem", "Level"
  FormatString = "(1(2X,I8),1(2X,I8))"
  DO iElem=1,nElems
    WRITE(UNIT_SCREEN,FormatString) iElem, MeshData_ElementsToLevel(iElem)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_ElementsToLevel
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_BCFacesToNodes()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToType
USE MOD_MeshMain_vars,ONLY: MeshData_BCFacesToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iBCFace
INTEGER :: nBCFaces
INTEGER :: BCFaceID
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nBCFaces = MeshArraysInfo%nBCFaces

IF (ALLOCATED(MeshData_BCFacesToNodes)) THEN
  DEALLOCATE(MeshData_BCFacesToNodes)
END IF
ALLOCATE(MeshData_BCFacesToNodes(1:4,1:nBCFaces))

IF (ALLOCATED(MeshData_BCFacesToType)) THEN
  DEALLOCATE(MeshData_BCFacesToType)
END IF
ALLOCATE(MeshData_BCFacesToType(1:nBCFaces))

BCFaceID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    IF (ASSOCIATED(aSide%BC) .EQV. .TRUE.) THEN
      BCFaceID = BCFaceID+1
      DO iNode=1,aSide%nNodes
        MeshData_BCFacesToNodes(iNode,BCFaceID) = aSide%Nodes(iNode)%Node%NodeID
      END DO
      MeshData_BCFacesToType(BCFaceID) = aSide%BC%BCType
    END IF
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: BCFaces-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "iBCFace", "NodeIDs"
  FormatString = "(1(2X,I8),4(2X,I8))"
  DO iBCFace=1,nBCFaces
    WRITE(UNIT_SCREEN,FormatString) iBCFace, MeshData_BCFacesToNodes(1:4,iBCFace)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_BCFacesToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_FacesToNodes()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tFaceNodes
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: tFaceNodesList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateFaceNodes
USE MOD_MeshMainMethods,ONLY: GetFaceNodesData
USE MOD_MeshMainMethods,ONLY: AddFaceNodesToList
USE MOD_MeshMainMethods,ONLY: DestructFaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nFaceNodes=4
INTEGER,PARAMETER :: nElemSides=6
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: nElems
INTEGER :: iFace
INTEGER :: ElemID
INTEGER :: FaceID
INTEGER :: nFaces
INTEGER :: NodeID(1:nFaceNodes)
INTEGER :: FaceNodes(1:nFaceNodes)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFaceNodes),POINTER :: aFaceNodes
TYPE(tFaceNodesList)     :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FacesToNodesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nElems = MeshArraysInfo%nElems

ALLOCATE(FacesToNodesArray(1:nElemSides*nElems,1:9))

FaceID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    FaceID = FaceID+1
    DO iNode=1,aSide%nNodes
      FaceNodes(iNode) = aSide%Nodes(iNode)%Node%NodeID
    END DO
    FacesToNodesArray(FaceID,5) = ElemID
    FacesToNodesArray(FaceID,6) = FaceNodes(1)
    FacesToNodesArray(FaceID,7) = FaceNodes(2)
    FacesToNodesArray(FaceID,8) = FaceNodes(3)
    FacesToNodesArray(FaceID,9) = FaceNodes(4)
    CALL QuickSort(FaceNodes)
    FacesToNodesArray(FaceID,1) = FaceNodes(1)
    FacesToNodesArray(FaceID,2) = FaceNodes(2)
    FacesToNodesArray(FaceID,3) = FaceNodes(3)
    FacesToNodesArray(FaceID,4) = FaceNodes(4)
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL QuickSortArray(FacesToNodesArray,5)

iFace  = 1
FaceID = 0
DO WHILE (iFace .LE. nElemSides*nElems)
  IF (iFace .GT. 1) THEN
    IF (ALL(FacesToNodesArray(iFace,1:4) .EQ. FacesToNodesArray(iFace-1,1:4))) THEN
      iFace = iFace+1
      CYCLE
    ELSE
      FaceID = FaceID+1
    END IF
  ELSE
    FaceID = FaceID+1
  END IF
  ! Store FaceID
  FaceNodes(1:4) = FacesToNodesArray(iFace,6:9)
  CALL CreateFaceNodes(aFaceNodes,FaceID,nFaceNodes,FaceNodes)
  CALL AddFaceNodesToList(aFaceNodes,FaceNodesList)
  iFace = iFace+1
END DO
nFaces = FaceID

! Faces-to-Nodes Array
IF (ALLOCATED(MeshData_FacesToNodes)) THEN
  DEALLOCATE(MeshData_FacesToNodes)
END IF
ALLOCATE(MeshData_FacesToNodes(1:nFaceNodes,1:nFaces))

aFaceNodes => FaceNodesList%FirstFaceNodes
DO WHILE(ASSOCIATED(aFaceNodes))
  CALL GetFaceNodesData(aFaceNodes,FaceID,NodeID)
  MeshData_FacesToNodes(1:nFaceNodes,FaceID) = NodeID(1:nFaceNodes)
  aFaceNodes => aFaceNodes%NextFaceNodes
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Faces-to-Nodes"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  FormatString = "(1(2X,I8),4(2X,I8))"
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "iFace", "NodeIDs"
  DO iFace=1,nFaces
    WRITE(UNIT_SCREEN,FormatString) iFace, MeshData_FacesToNodes(1:4,iFace)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

DEALLOCATE(FacesToNodesArray)
CALL DestructFaceNodesList(FaceNodesList)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_FacesToNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateArray3D_ElementsToFaces()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshArraysInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToFaces
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
USE MOD_DataStructures,ONLY: QuickSortArray
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: nFaceNodes = 4
INTEGER,PARAMETER :: nElemSides = 6
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: nElems
INTEGER :: iElem
INTEGER :: iFace
INTEGER :: ElemID
INTEGER :: FaceID
INTEGER :: LocSide
INTEGER :: FaceNodes(1:nFaceNodes)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FacesToNodesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nElems = MeshArraysInfo%nElems

IF (ALLOCATED(MeshData_ElementsToFaces)) THEN
  DEALLOCATE(MeshData_ElementsToFaces)
END IF
ALLOCATE(MeshData_ElementsToFaces(1:nElemSides,1:nElems))
ALLOCATE(FacesToNodesArray(1:nElemSides*nElems,1:6))

iFace = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    LocSide = aSide%LocSide
    iFace = iFace+1
    DO iNode=1,aSide%nNodes
      FaceNodes(iNode) = aSide%Nodes(iNode)%Node%NodeID
    END DO
    CALL QuickSort(FaceNodes)
    FacesToNodesArray(iFace,1) = FaceNodes(1)
    FacesToNodesArray(iFace,2) = FaceNodes(2)
    FacesToNodesArray(iFace,3) = FaceNodes(3)
    FacesToNodesArray(iFace,4) = FaceNodes(4)
    FacesToNodesArray(iFace,5) = ElemID
    FacesToNodesArray(iFace,6) = LocSide
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL QuickSortArray(FacesToNodesArray,5)

iFace  = 1
FaceID = 0
DO WHILE (iFace .LE. nElemSides*nElems)
  ElemID  = FacesToNodesArray(iFace,5)
  LocSide = FacesToNodesArray(iFace,6)
  IF (iFace .GT. 1) THEN
    IF (ALL(FacesToNodesArray(iFace,1:4) .EQ. FacesToNodesArray(iFace-1,1:4))) THEN
      MeshData_ElementsToFaces(LocSide,ElemID) = FaceID
      iFace = iFace+1
      CYCLE
    ELSE
      FaceID = FaceID+1
    END IF
  ELSE
    FaceID = FaceID+1
  END IF
  MeshData_ElementsToFaces(LocSide,ElemID) = FaceID
  iFace = iFace+1
END DO

DEALLOCATE(FacesToNodesArray)

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Printing Array: Elements-to-Faces"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2(2X,A8))") "iElem", "SideIDs"
  FormatString = "(1(2X,I8),6(2X,I8))"
  DO iElem=1,nElems
    WRITE(UNIT_SCREEN,FormatString) iElem, MeshData_ElementsToFaces(1:6,iElem)
  END DO
  WRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateArray3D_ElementsToFaces
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshExportElementsList
!======================================================================================================================!
