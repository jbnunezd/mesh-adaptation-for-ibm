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
MODULE MOD_MeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshElementsList
  MODULE PROCEDURE InitializeMeshElementsList
END INTERFACE

INTERFACE CreateElementsList
  MODULE PROCEDURE CreateElementsList
END INTERFACE

INTERFACE SetUniqueElemID
  MODULE PROCEDURE SetUniqueElemID
END INTERFACE

INTERFACE SetUniqueNodes
  MODULE PROCEDURE SetUniqueNodes
END INTERFACE

INTERFACE SetUniqueSideID
  MODULE PROCEDURE SetUniqueSideID
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshElementsList
PUBLIC :: CreateElementsList
PUBLIC :: SetUniqueElemID
PUBLIC :: SetUniqueSideID
PUBLIC :: SetUniqueNodes
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
SUBROUTINE InitializeMeshElementsList()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
USE MOD_MeshElementsList_vars,ONLY: InitializeMeshElementsListIsDone
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshElementsListIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshElementsList not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESH ELEMENT LIST MODULE..."
CALL PrintHeader(Header)

ParametersMeshElementsList%DebugMeshElementsList = GetLogical('DebugMeshElementsList','.FALSE.')

InitializeMeshElementsListIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshElementsList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElementsList()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshAdapter_vars,ONLY: ParametersMeshAdapter
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

SELECT CASE(LowerCase(ParametersMeshAdapter%WhichMeshConstructionMethod))
  CASE('mesh-builtin')
    CALL CreateElementsListFromMeshBuiltIn3D()
  CASE DEFAULT
  ErrorMessage = "CreateElementsList: Unknown Mesh Creation Procedure"
  CALL PrintError(__STAMP__,ErrorMessage)  
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElementsList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateElementsListFromMeshBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: nBoxElems3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList_vars,ONLY: ParametersMeshElementsList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: MeshData_BCIndex
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryType
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CreateBC
USE MOD_MeshMainMethods,ONLY: CreateElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: AddDataToNode
USE MOD_MeshMainMethods,ONLY: AddElemToList
USE MOD_MeshMainMethods,ONLY: CreateHexaElem
USE MOD_MeshMainMethods,ONLY: PrintElemList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
INTEGER :: iNode
INTEGER :: iSide
INTEGER :: ElemID
INTEGER :: NodeID
INTEGER :: nElems
INTEGER :: nNodes
LOGICAL :: IsOnBoundary
REAL    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER        :: aElem
TYPE(tSide),POINTER        :: aSide
TYPE(tNodePtr),ALLOCATABLE :: MeshNodes(:)
TYPE(tNodePtr),ALLOCATABLE :: Nodes(:,:,:)
!----------------------------------------------------------------------------------------------------------------------!

nElems = MeshInfo%nElems
nNodes = MeshInfo%nNodes

ALLOCATE(MeshNodes(1:nNodes))
ALLOCATE(Nodes(0:PP_NGeo,0:PP_NGeo,0:PP_NGeo))

! Create MeshNodes pointers
ElemID = 0
NodeID = 0
DO kk=1,nBoxElems3D(3)
  DO jj=1,nBoxElems3D(2)
    DO ii=1,nBoxElems3D(1)
      ElemID = ElemID+1
      DO k=0,PP_NGeo
        DO j=0,PP_NGeo
          DO i=0,PP_NGeo
            ! Create Pointers to Mesh Nodes
            NodeID = NodeID+1
            Coords(1:3) = MeshData_ElementsToNodesCoordinates3D(1:3,i,j,k,ElemID)
            CALL CreateNode(MeshNodes(NodeID)%Node)
            CALL AddDataToNode(MeshNodes(NodeID)%Node,NodeID,Coords(1:3))
            
            ! Flag Boundary Sides (CGNS convention)
            MeshNodes(NodeID)%Node%BCFlag = 0
            ! CGNS Face 1: Z-
            IF ((kk .EQ. 1) .AND. (k .EQ. 0)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+1
            END IF
            ! CGNS Face 2: Y-
            IF ((jj .EQ. 1) .AND. (j .EQ. 0)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+20
            END IF
            ! CGNS Face 3: X+
            IF ((ii .EQ. nBoxElems3D(1)) .AND. (i .EQ. PP_NGeo)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+300
            END IF
            ! CGNS Face 4: Y+
            IF ((jj .EQ. nBoxElems3D(2)) .AND. (j .EQ. PP_NGeo)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+4000
            END IF
            ! CGNS Face 5: X-
            IF ((ii .EQ. 1) .AND. (i .EQ. 0)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+50000
            END IF
            ! CGNS Face 6: Z+
            IF ((kk .EQ. nBoxElems3D(3)) .AND. (k .EQ. PP_NGeo)) THEN
              MeshNodes(NodeID)%Node%BCFlag = MeshNodes(NodeID)%Node%BCFlag+600000
            END IF
          END DO
        END DO
      END DO
    END DO
  END DO
END DO

! Create Elements and Sides
ElemID = 0
NodeID = 0
DO kk=1,nBoxElems3D(3)
  DO jj=1,nBoxElems3D(2)
    DO ii=1,nBoxElems3D(1)
      ElemID = ElemID+1
      DO k=0,PP_NGeo
        DO j=0,PP_NGeo
          DO i=0,PP_NGeo
            NodeID = NodeID+1
            Nodes(i,j,k)%Node => MeshNodes(NodeID)%Node
          END DO
        END DO
      END DO
      CALL CreateHexaElem(aElem,PP_NGeo,Nodes,ElemID)
      CALL AddElemToList(aElem,ElemList)
    END DO
  END DO
END DO

IF (ALLOCATED(MeshNodes)) THEN
  DEALLOCATE(MeshNodes)
END IF
IF (ALLOCATED(Nodes)) THEN
  DEALLOCATE(Nodes)  
END IF
IF (ALLOCATED(MeshData_ElementsToNodesCoordinates3D)) THEN
  DEALLOCATE(MeshData_ElementsToNodesCoordinates3D)  
END IF

! Add Boundary Condition Information to Sides
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE(ASSOCIATED(aSide))
    DO iSide=1,6
      IsOnBoundary = .TRUE.
      DO iNode=1,4      
        IF ((MOD(aSide%Nodes(iNode)%Node%BCFlag,10**iSide)/(10**(iSide-1))) .NE. iSide) THEN
          IsOnBoundary = .FALSE.
          EXIT
        END IF
      END DO
      IF (MeshData_BCIndex(iSide) .EQ. 0) THEN
        IsOnBoundary = .FALSE.
      END IF
      IF (IsOnBoundary .EQV. .TRUE.) THEN
        CALL CreateBC(aSide%BC)
        aSide%BC%BCIndex    = MeshData_BCIndex(iSide)
        aSide%BC%BCType     = MeshData_BoundaryType(aSide%BC%BCIndex,1)
        aSide%BC%BCState    = MeshData_BoundaryType(aSide%BC%BCIndex,2)
        aSide%BC%BCalphaInd = MeshData_BoundaryType(aSide%BC%BCIndex,3)
      END IF
    END DO
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL SetUniqueNodes()
CALL SetUniqueElemID()
CALL SetUniqueSideID()

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "Printing Elements-Nodes List and Local Faces-Nodes List"
  WRITE(UNIT_SCREEN,*) "======================================================="
  CALL PrintElemList()
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateElementsListFromMeshBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetUniqueElemID()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

ElemID = 0
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = ElemID+1
  aElem%ElemID = ElemID
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetUniqueElemID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetUniqueSideID()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
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
INTEGER :: iNode
INTEGER :: iFace
INTEGER :: ElemID
INTEGER :: FaceID
INTEGER :: nElems
INTEGER :: nFaceNodes
INTEGER :: nElemSides
INTEGER :: LocSide
INTEGER,ALLOCATABLE :: FaceNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: FacesToNodesArray1(:,:)
INTEGER,ALLOCATABLE :: FacesToNodesArray2(:,:)
!----------------------------------------------------------------------------------------------------------------------!

! Count Elements
CALL CountElems(ElemList,nElems)

nFaceNodes = 4
nElemSides = 6

ALLOCATE(FacesToNodesArray1(1:nElemSides*nElems,1:nElemSides))
ALLOCATE(FacesToNodesArray2(1:nElemSides,1:nElems))
ALLOCATE(FaceNodes(1:nFaceNodes))

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
    FacesToNodesArray1(iFace,1:nFaceNodes) = FaceNodes(1:nFaceNodes)
    FacesToNodesArray1(iFace,nFaceNodes+1) = ElemID
    FacesToNodesArray1(iFace,nFaceNodes+2) = LocSide
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

CALL QuickSortArray(FacesToNodesArray1,nFaceNodes+1)

iFace  = 1
FaceID = 0
DO WHILE (iFace .LE. nElemSides*nElems)
  ElemID  = FacesToNodesArray1(iFace,nFaceNodes+1)
  LocSide = FacesToNodesArray1(iFace,nFaceNodes+2)
  IF (iFace .GT. 1) THEN
    IF (ALL(FacesToNodesArray1(iFace,1:nFaceNodes) .EQ. FacesToNodesArray1(iFace-1,1:nFaceNodes))) THEN
      FacesToNodesArray2(LocSide,ElemID) = FaceID
      iFace = iFace+1
      CYCLE
    ELSE
      FaceID = FaceID+1
    END IF
  ELSE
    FaceID = FaceID+1
  END IF
  FacesToNodesArray2(LocSide,ElemID) = FaceID
  iFace = iFace+1
END DO

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    aSide%SideID = FacesToNodesArray2(aSide%LocSide,aElem%ElemID)
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

IF (ALLOCATED(FacesToNodesArray1)) THEN
  DEALLOCATE(FacesToNodesArray1)  
END IF
IF (ALLOCATED(FacesToNodesArray2)) THEN
  DEALLOCATE(FacesToNodesArray2)  
END IF

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "FUNCTION: SET_UNIQUE_SIDEID"
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nFaceID       : ", FaceID
  WRITE(UNIT_SCREEN,*) "======================================================="
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetUniqueSideID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetUniqueNodes()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tNode
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
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
INTEGER :: iNode
INTEGER :: jNode
INTEGER :: NodeID
INTEGER :: nNodes
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tNode),POINTER :: aNode
!----------------------------------------------------------------------------------------------------------------------!
INTEGER                    :: nDeletedNodes
REAL                       :: Point1(1:3)
REAL                       :: Point2(1:3)
REAL,ALLOCATABLE           :: NodesCoordinates(:,:)
TYPE(tNodePtr),ALLOCATABLE :: NodesList(:)
!----------------------------------------------------------------------------------------------------------------------!

! Set marker (tmp) to zero
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SetNodesMarkerToZero(aElem)
  aElem => aElem%NextElem
END DO

! Set marker (tmp) to NodeID
NodeID = 0
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SetNodesMarkerToNodeID(aElem,NodeID)
  aElem => aElem%NextElem
END DO

! Create list of nodes
nNodes = NodeID
ALLOCATE(NodesList(1:nNodes))
DO iNode=1,nNodes
  NULLIFY(NodesList(iNode)%Node)
END DO

! Save all nodes to nodes list
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SaveNodesToNodesList(aElem,NodesList)
  aElem => aElem%NextElem
END DO

! Create nodes coordinates list to remove duplicated nodes
ALLOCATE(NodesCoordinates(1:nNodes,1:4))
DO iNode=1,nNodes
  aNode => NodesList(iNode)%Node
  NodesCoordinates(iNode,1) = aNode%Coords(1)
  NodesCoordinates(iNode,2) = aNode%Coords(2)
  NodesCoordinates(iNode,3) = aNode%Coords(3)
  NodesCoordinates(iNode,4) = REAL(aNode%tmp)
END DO
CALL QuickSortArray(NodesCoordinates,4)

DO iNode=1,nNodes
  NodesList(iNode)%Node%tmp = 0
END DO

NodeID = 0
nDeletedNodes = 0
DO iNode=1,nNodes
  jNode = INT(NodesCoordinates(iNode,4))
  IF (iNode .GT. 1) THEN
    Point1(1:3) = NodesCoordinates(iNode  ,1:3)
    Point2(1:3) = NodesCoordinates(iNode-1,1:3)
    IF (ComparePoints(Point1,Point2) .EQV. .TRUE.) THEN
      nDeletedNodes = nDeletedNodes+1
      NodesCoordinates(iNode,4) = NodesCoordinates(iNode-1,4)
      NodesList(jNode)%Node%tmp    = INT(NodesCoordinates(iNode,4))
      NodesList(jNode)%Node%NodeID = NodeID
      CYCLE
    ELSE
      NodeID = NodeID+1
    END IF
  ELSE
    NodeID = NodeID+1
  END IF
  NodesList(jNode)%Node%tmp    = INT(NodesCoordinates(iNode,4))
  NodesList(jNode)%Node%NodeID = NodeID
END DO

! Nodes point now to nodes with new NodeIDs
aElem => ElemList%FirstElem
DO WHILE(ASSOCIATED(aElem))
  CALL SetNodesToNewNodes(aElem,NodesList)
  aElem => aElem%NextElem
END DO

IF (ALLOCATED(NodesCoordinates)) THEN
  DEALLOCATE(NodesCoordinates)  
END IF
IF (ALLOCATED(NodesList)) THEN
  DEALLOCATE(NodesList)  
END IF

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,*) "FUNCTION: SET_UNIQUE_NODES"
  WRITE(UNIT_SCREEN,*) "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nNodes        : ", nNodes
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nDeletedNodes : ", nDeletedNodes
  WRITE(UNIT_SCREEN,"(2X,A,I0)") "nUniqueNodes  : ", nNodes-nDeletedNodes
  WRITE(UNIT_SCREEN,*) "======================================================="
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetUniqueNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetNodesMarkerToZero(aElem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  aElem%Nodes(iNode)%Node%tmp = 0
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    aSide%Nodes(iNode)%Node%tmp = 0
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetNodesMarkerToZero
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetNodesMarkerToNodeID(aElem,NodeID)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: SetAndCountNodeID
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
INTEGER,INTENT(INOUT)             :: NodeID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  CALL SetAndCountNodeID(aElem%Nodes(iNode)%Node%tmp,NodeID)
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    CALL SetAndCountNodeID(aSide%Nodes(iNode)%Node%tmp,NodeID)
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetNodesMarkerToNodeID
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SaveNodesToNodesList(aElem,NodesList)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
TYPE(tNodePtr),INTENT(INOUT)      :: NodesList(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  NodesList(aElem%Nodes(iNode)%Node%tmp)%Node => aElem%Nodes(iNode)%Node
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    NodesList(aSide%Nodes(iNode)%Node%tmp)%Node => aSide%Nodes(iNode)%Node
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SaveNodesToNodesList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SetNodesToNewNodes(aElem,NodesList)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tNodePtr
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: aElem
TYPE(tNodePtr),INTENT(INOUT)      :: NodesList(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!

DO iNode=1,aElem%nNodes
  aElem%Nodes(iNode)%Node => NodesList(aElem%Nodes(iNode)%Node%tmp)%Node
END DO

aSide => aElem%FirstSide
DO WHILE(ASSOCIATED(aSide))
  DO iNode=1,aSide%nNodes
    aSide%Nodes(iNode)%Node => NodesList(aSide%Nodes(iNode)%Node%tmp)%Node 
  END DO
  aSide => aSide%NextElemSide
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SetNodesToNewNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION ComparePoints(Point1,Point2) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN) :: Point1(1:3)
REAL,INTENT(IN) :: Point2(1:3)
LOGICAL         :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL            :: ds
!----------------------------------------------------------------------------------------------------------------------!

ds = SQRT(DOT_PRODUCT(Point1-Point2,Point1-Point2))
Flag = .FALSE.
IF (ds .LT. PP_ACCURACY) THEN
  Flag = .TRUE.
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION ComparePoints
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshElementsList
!======================================================================================================================!
