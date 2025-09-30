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
MODULE MOD_MeshRefinementConnectivity
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateNonConformingConnectivity
  MODULE PROCEDURE CreateNonConformingConnectivity
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: CreateNonConformingConnectivity
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
SUBROUTINE CreateNonConformingConnectivity()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: IsotropicRefinement
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: NonConformingSidesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!

CALL GetNonConformingSidesArray(NonConformingSidesArray)
IF (IsotropicRefinement .EQV. .FALSE.) THEN
  CALL GetQuad2QuadArray(NonConformingSidesArray)
ELSE
  CALL GetQuad4QuadArray(NonConformingSidesArray)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateNonConformingConnectivity
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetNonConformingSidesArray(NonConformingSidesArray)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tArrayINTEGER
USE MOD_DataStructures,ONLY: tLinkedList
USE MOD_DataStructures,ONLY: tLinkedListNode
USE MOD_DataStructures,ONLY: CreateLinkedListNode
USE MOD_DataStructures,ONLY: AddLinkedListNode
USE MOD_DataStructures,ONLY: GetLinkedListNode
USE MOD_DataStructures,ONLY: PrintLinkedList
USE MOD_DataStructures,ONLY: DestructLinkedList
USE MOD_DataStructures,ONLY: CountLinkedListNodes
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
INTEGER,ALLOCATABLE,INTENT(OUT) :: NonConformingSidesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNonConformingSide
INTEGER :: nNonConformingSide
INTEGER :: SideData(1:7)
INTEGER :: Ordering(1:7)
INTEGER :: Data1
INTEGER :: Data2
INTEGER :: iSide
INTEGER :: BucketID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: DATA_ElemID
INTEGER :: DATA_TreeID
INTEGER :: DATA_ParentID
INTEGER :: DATA_Level
INTEGER :: DATA_LocChild
INTEGER :: DATA_LocSide
INTEGER :: DATA_SideID
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tArrayINTEGER)           :: aSideData
TYPE(tLinkedList)             :: NonConformingSidesList
TYPE(tLinkedListNode),POINTER :: aNonConformingSide
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

DATA_SideID   = 1
DATA_LocSide  = 2
DATA_ParentID = 3
DATA_Level    = 4
DATA_LocChild = 5
DATA_TreeID   = 6
DATA_ElemID   = 7

ALLOCATE(aSideData%Data(1:SIZE(SideData)))

! Extracting side information from each element
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  aSide => aElem%FirstSide
  DO WHILE (ASSOCIATED(aSide))
    IF (ASSOCIATED(aSide%BC) .EQV. .FALSE.) THEN
      SideData(DATA_SideID)   = aSide%SideID
      SideData(DATA_LocSide)  = aSide%LocSide
      SideData(DATA_ParentID) = aElem%ParentID
      SideData(DATA_Level)    = aElem%Level
      SideData(DATA_LocChild) = aElem%LocChild
      SideData(DATA_TreeID)   = aElem%TreeID
      SideData(DATA_ElemID)   = aElem%ElemID
      BucketID = aSide%SideID
      aSideData%Data(1:7) = SideData(1:7)
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,NonConformingSidesList)
    END IF
    aSide => aSide%NextElemSide
  END DO
  aElem => aElem%NextElem
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(NonConformingSidesList,nNonConformingSide)

! Allocating array of inner sides (conforming and non-conforming)
IF (ALLOCATED(NonConformingSidesArray)) THEN
  DEALLOCATE(NonConformingSidesArray)
END IF
ALLOCATE(NonConformingSidesArray(1:nNonConformingSide,1:7))

! Exporting inner sides (conforming and non-conforming) data from list to array
iNonConformingSide = 0
aNonConformingSide => NonConformingSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNonConformingSide))
  iNonConformingSide = iNonConformingSide+1
  CALL GetLinkedListNode(aNonConformingSide,BucketID,aSideData)
  NonConformingSidesArray(iNonConformingSide,1:7) = aSideData%Data(1:7)
  aNonConformingSide => aNonConformingSide%NextLinkedListNode
END DO

CALL DestructLinkedList(NonConformingSidesList)

! Sorting array of inner sides (conforming and non-conforming)
CALL QuickSortArray(NonConformingSidesArray,2)

! Removing repeated sides (only conforming sides)
iNonConformingSide = 1
DO WHILE (iNonConformingSide .LE. nNonConformingSide)
  IF (iNonConformingSide .EQ. 1) THEN
    Data1 = NonConformingSidesArray(iNonConformingSide+0,1)
    Data2 = NonConformingSidesArray(iNonConformingSide+1,1)
    IF (ABS(Data2-Data1) .EQ. 0) THEN
      iNonConformingSide = iNonConformingSide+2
      CYCLE
    END IF
    SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
    BucketID = iNonConformingSide
    aSideData%Data(1:7) = SideData(1:7)
    CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
    CALL AddLinkedListNode(aNonConformingSide,NonConformingSidesList)
    iNonConformingSide = iNonConformingSide+1
  ELSEIF ((iNonConformingSide .GT. 1) .AND. (iNonConformingSide .LT. nNonConformingSide)) THEN
    Data1 = NonConformingSidesArray(iNonConformingSide-1,1)
    Data2 = NonConformingSidesArray(iNonConformingSide+0,1)
    IF (ABS(Data2-Data1) .EQ. 0) THEN
      iNonConformingSide = iNonConformingSide+1
      CYCLE
    END IF
    Data1 = NonConformingSidesArray(iNonConformingSide+0,1)
    Data2 = NonConformingSidesArray(iNonConformingSide+1,1)
    IF (ABS(Data2-Data1) .EQ. 0) THEN
      iNonConformingSide = iNonConformingSide+1
      CYCLE
    END IF
    SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
    aSideData%Data(1:7) = SideData(1:7)
    BucketID = iNonConformingSide
    CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
    CALL AddLinkedListNode(aNonConformingSide,NonConformingSidesList)
    iNonConformingSide = iNonConformingSide+1
  ELSEIF (iNonConformingSide .EQ. nNonConformingSide) THEN
    Data1 = NonConformingSidesArray(iNonConformingSide-1,1)
    Data2 = NonConformingSidesArray(iNonConformingSide+0,1)
    IF (ABS(Data2-Data1) .EQ. 0) THEN
      iNonConformingSide = iNonConformingSide+1
      CYCLE
    END IF
    SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
    aSideData%Data(1:7) = SideData(1:7)
    BucketID = iNonConformingSide
    CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
    CALL AddLinkedListNode(aNonConformingSide,NonConformingSidesList)
    iNonConformingSide = iNonConformingSide+1
  END IF
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(NonConformingSidesList,nNonConformingSide)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(NonConformingSidesArray)) THEN
  DEALLOCATE(NonConformingSidesArray)
END IF
ALLOCATE(NonConformingSidesArray(1:nNonConformingSide,1:7))

! Exporting inner sides (non-conforming) data from list to array
Ordering = (/DATA_TreeID,DATA_ParentID,DATA_Level,DATA_LocSide,DATA_LocChild,DATA_ElemID,DATA_SideID/)
iNonConformingSide = 0
aNonConformingSide => NonConformingSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNonConformingSide))
  iNonConformingSide = iNonConformingSide+1
  CALL GetLinkedListNode(aNonConformingSide,BucketID,aSideData)
  NonConformingSidesArray(iNonConformingSide,1:7) = aSideData%Data(Ordering)
  aNonConformingSide => aNonConformingSide%NextLinkedListNode
END DO

DATA_TreeID   = 1
DATA_ParentID = 2
DATA_Level    = 3
DATA_LocSide  = 4
DATA_LocChild = 5
DATA_ElemID   = 6
DATA_SideID   = 7

! Sorting array of inner sides (non-conforming)
CALL QuickSortArray(NonConformingSidesArray,5)

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "NonConformingSidesArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  FormatString = "(8(2X,I8),4(2X,I8))"
  WRITE(UNIT_SCREEN,"(9(2X,A8))") "iSide", "TreeID", "ParentID", "Level", "LocSide", "LocChild", "ElemID", "SideID", "NodeIDs"
  IF (SIZE(NonConformingSidesArray,1) .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in NonConformingSideArray ***"
  END IF
  DO iNonConformingSide=1,SIZE(NonConformingSidesArray,1)
    iSide = NonConformingSidesArray(iNonConformingSide,DATA_SideID)
    WRITE(UNIT_SCREEN,FormatString) &
      iNonConformingSide, &
      NonConformingSidesArray(iNonConformingSide,1:7), &
      MeshData_FacesToNodes(:,iSide)
  END DO
END IF

CALL DestructLinkedList(NonConformingSidesList)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetNonConformingSidesArray
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetQuad2QuadArray(NonConformingSidesArray)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tBucket
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tArrayINTEGER
USE MOD_DataStructures,ONLY: tLinkedList
USE MOD_DataStructures,ONLY: tLinkedListNode
USE MOD_DataStructures,ONLY: CreateLinkedListNode
USE MOD_DataStructures,ONLY: AddLinkedListNode
USE MOD_DataStructures,ONLY: GetLinkedListNode
USE MOD_DataStructures,ONLY: PrintLinkedList
USE MOD_DataStructures,ONLY: DestructLinkedList
USE MOD_DataStructures,ONLY: CountLinkedListNodes
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
INTEGER,ALLOCATABLE,INTENT(INOUT) :: NonConformingSidesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: DATA_ElemID
INTEGER :: DATA_TreeID
INTEGER :: DATA_ParentID
INTEGER :: DATA_Level
INTEGER :: DATA_LocChild
INTEGER :: DATA_LocSide
INTEGER :: DATA_SideID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nData
INTEGER :: BucketID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: TreeID1
INTEGER :: TreeID2
INTEGER :: ParentID1
INTEGER :: ParentID2
INTEGER :: LocChild1
INTEGER :: LocChild2
INTEGER :: LocSide1
INTEGER :: LocSide2
INTEGER :: LocSide
LOGICAL :: Flag
LOGICAL :: Flag1
LOGICAL :: Flag2
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iSide
INTEGER :: SideID
INTEGER :: iQuad2Quad
INTEGER :: nQuad2Quad
INTEGER :: iNonConformingSide
INTEGER :: nNonConformingSide
INTEGER :: NodeID(1:4)
INTEGER :: SideData(1:7)
INTEGER :: SideID_in(1:2)
INTEGER :: NodeID_in(1:2,1:4)
INTEGER :: NodeID_out(1:6)
INTEGER :: QuadData(1:6+2)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: MasterSideAUX(:,:)
INTEGER,ALLOCATABLE :: Quad2QuadAUX(:,:)
INTEGER,ALLOCATABLE :: Quad2QuadArray(:,:)
INTEGER,ALLOCATABLE :: MasterSidesArray(:,:)
INTEGER,ALLOCATABLE :: SlaveSidesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE     :: HashTable(:)
TYPE(tLinkedList)             :: SlaveSidesList
TYPE(tLinkedList)             :: MasterSidesList
TYPE(tLinkedList)             :: Quad2QuadList
TYPE(tArrayINTEGER)           :: aSideData
TYPE(tArrayINTEGER)           :: aQuadData
TYPE(tLinkedListNode),POINTER :: aQuad2Quad
TYPE(tLinkedListNode),POINTER :: aNonConformingSide
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

DATA_TreeID   = 1
DATA_ParentID = 2
DATA_Level    = 3
DATA_LocSide  = 4
DATA_LocChild = 5
DATA_ElemID   = 6
DATA_SideID   = 7

ALLOCATE(aQuadData%Data(1:SIZE(QuadData)))
ALLOCATE(aSideData%Data(1:SIZE(SideData)))

! Creating HashTable of non-conforming sides
nData = 4
nNonConformingSide = SIZE(NonConformingSidesArray,1)
CALL CreateHashTable(HashTable,nNonConformingSide,nData)

DO iNonConformingSide=1,nNonConformingSide
  SideID      = NonConformingSidesArray(iNonConformingSide,DATA_SideID)
  NodeID(1:4) = MeshData_FacesToNodes(1:4,SideID)
  CALL AddBucketIDToHashTable(HashTable,SideID,NodeID)
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Non-Conforming Sides"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(HashTable)
END IF

! Creating MasterSidesList and SlaveSidesList
iNonConformingSide = 1
DO WHILE (iNonConformingSide .LE. nNonConformingSide)
  IF ((iNonConformingSide .GE. 1) .AND. (iNonConformingSide .LE. nNonConformingSide-1)) THEN
    Flag  = .FALSE.
    Flag1 = .FALSE.
    Flag2 = .FALSE.
    TreeID1   = NonConformingSidesArray(iNonConformingSide+0,DATA_TreeID)
    TreeID2   = NonConformingSidesArray(iNonConformingSide+1,DATA_TreeID)
    ParentID1 = NonConformingSidesArray(iNonConformingSide+0,DATA_ParentID)
    ParentID2 = NonConformingSidesArray(iNonConformingSide+1,DATA_ParentID)
    LocSide1  = NonConformingSidesArray(iNonConformingSide+0,DATA_LocSide)
    LocSide2  = NonConformingSidesArray(iNonConformingSide+1,DATA_LocSide)
    LocChild1 = NonConformingSidesArray(iNonConformingSide+0,DATA_LocChild)
    LocChild2 = NonConformingSidesArray(iNonConformingSide+1,DATA_LocChild)
    IF (TreeID1 .EQ. TreeID2) THEN
      IF (ParentID1 .EQ. ParentID2) THEN
        IF (LocSide1 .EQ. LocSide2) THEN
          SideID_in(1:2)   = NonConformingSidesArray(iNonConformingSide:iNonConformingSide+1,DATA_SideID)
          NodeID_in(1,1:4) = MeshData_FacesToNodes(1:4,SideID_in(1))
          NodeID_in(2,1:4) = MeshData_FacesToNodes(1:4,SideID_in(2))
          CALL CreateQuad2QuadNodes(LocSide1,NodeID_in(1:2,1:4),NodeID_out(1:6))
          Flag1 = CheckBucketIDIsInHashTable(HashTable,NodeID_out(1:4))
          Flag2 = CheckLocSideLocChild3D_XY(LocSide1,(/LocChild1,LocChild2/))
          Flag  = (Flag1 .AND. Flag2)
        END IF
      END IF
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
    ELSE
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,MasterSidesList)
    END IF
    iNonConformingSide = iNonConformingSide+1
  ELSE
    Flag  = .FALSE.
    Flag1 = .FALSE.
    Flag2 = .FALSE.
    TreeID1   = NonConformingSidesArray(iNonConformingSide-1,DATA_TreeID)
    TreeID2   = NonConformingSidesArray(iNonConformingSide-0,DATA_TreeID)
    ParentID1 = NonConformingSidesArray(iNonConformingSide-1,DATA_ParentID)
    ParentID2 = NonConformingSidesArray(iNonConformingSide-0,DATA_ParentID)
    LocSide1  = NonConformingSidesArray(iNonConformingSide-1,DATA_LocSide)
    LocSide2  = NonConformingSidesArray(iNonConformingSide-0,DATA_LocSide)
    LocChild1 = NonConformingSidesArray(iNonConformingSide-1,DATA_LocChild)
    LocChild2 = NonConformingSidesArray(iNonConformingSide-0,DATA_LocChild)
    IF (TreeID1 .EQ. TreeID2) THEN
      IF (ParentID1 .EQ. ParentID2) THEN
        IF (LocSide1 .EQ. LocSide2) THEN
          SideID_in(1:2)   = NonConformingSidesArray(iNonConformingSide-1:iNonConformingSide,DATA_SideID)
          NodeID_in(1,1:4) = MeshData_FacesToNodes(1:4,SideID_in(1))
          NodeID_in(2,1:4) = MeshData_FacesToNodes(1:4,SideID_in(2))
          CALL CreateQuad2QuadNodes(LocSide1,NodeID_in(1:2,1:4),NodeID_out(1:6))
          Flag1 = CheckBucketIDIsInHashTable(HashTable,NodeID_out(1:4))
          Flag2 = CheckLocSideLocChild3D_XY(LocSide1,(/LocChild1,LocChild2/))
          Flag  = (Flag1 .AND. Flag2)
        END IF
      END IF
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
    ELSE
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,MasterSidesList)
    END IF
    iNonConformingSide = iNonConformingSide+1
  END IF
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(MasterSidesList,nNonConformingSide)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(MasterSidesArray)) THEN
  DEALLOCATE(MasterSidesArray)
END IF
ALLOCATE(MasterSidesArray(1:nNonConformingSide,1:7))

! Exporting inner sides (non-conforming) data from list to array
iNonConformingSide = 0
aNonConformingSide => MasterSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNonConformingSide))
  iNonConformingSide = iNonConformingSide+1
  CALL GetLinkedListNode(aNonConformingSide,BucketID,aSideData)
  MasterSidesArray(iNonConformingSide,1:7) = aSideData%Data(1:7)
  aNonConformingSide => aNonConformingSide%NextLinkedListNode
END DO

! Sorting array of inner sides (non-conforming)
CALL QuickSortArray(MasterSidesArray,5)

! Counting number of entries in list
CALL CountLinkedListNodes(SlaveSidesList,nNonConformingSide)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(SlaveSidesArray)) THEN
  DEALLOCATE(SlaveSidesArray)
END IF
ALLOCATE(SlaveSidesArray(1:nNonConformingSide,1:7))

! Exporting inner sides (non-conforming) data from list to array
iNonConformingSide = 0
aNonConformingSide => SlaveSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNonConformingSide))
  iNonConformingSide = iNonConformingSide+1
  CALL GetLinkedListNode(aNonConformingSide,BucketID,aSideData)
  SlaveSidesArray(iNonConformingSide,1:7) = aSideData%Data(1:7)
  aNonConformingSide => aNonConformingSide%NextLinkedListNode
END DO

! Sorting array of inner sides (non-conforming)
CALL QuickSortArray(SlaveSidesArray,5)

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "MasterSidesArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  FormatString = "(8(2X,I8),4(2X,I8))"
  WRITE(UNIT_SCREEN,"(9(2X,A8))") "iSide", "TreeID", "ParentID", "Level", "LocSide", "LocChild", "ElemID", "SideID", "NodeIDs"
  IF (SIZE(MasterSidesArray,1) .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in NonConformingSideArray ***"
  END IF
  DO iNonConformingSide=1,SIZE(MasterSidesArray,1)
    iSide = MasterSidesArray(iNonConformingSide,DATA_SideID)
    WRITE(UNIT_SCREEN,FormatString) &
      iNonConformingSide, &
      MasterSidesArray(iNonConformingSide,1:7), &
      MeshData_FacesToNodes(:,iSide)
  END DO
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "SlaveSidesArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  FormatString = "(8(2X,I8),4(2X,I8))"
  WRITE(UNIT_SCREEN,"(9(2X,A8))") "iSide", "TreeID", "ParentID", "Level", "LocSide", "LocChild", "ElemID", "SideID", "NodeIDs"
  IF (SIZE(SlaveSidesArray,1) .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in NonConformingSideArray ***"
  END IF
  DO iNonConformingSide=1,SIZE(SlaveSidesArray,1)
    iSide = SlaveSidesArray(iNonConformingSide,DATA_SideID)
    WRITE(UNIT_SCREEN,FormatString) &
      iNonConformingSide, &
      SlaveSidesArray(iNonConformingSide,1:7), &
      MeshData_FacesToNodes(:,iSide)
  END DO
END IF

CALL DestructLinkedList(MasterSidesList)
CALL DestructLinkedList(SlaveSidesList)

nQuad2Quad = SIZE(SlaveSidesArray,1)

! Removing repeated sides (only conforming sides)
BucketID = 0
iQuad2Quad = 1
DO WHILE (iQuad2Quad .LE. nQuad2Quad)
  Flag = .FALSE.
  TreeID1   = SlaveSidesArray(iQuad2Quad+0,DATA_TreeID)
  TreeID2   = SlaveSidesArray(iQuad2Quad+1,DATA_TreeID)
  ParentID1 = SlaveSidesArray(iQuad2Quad+0,DATA_ParentID)
  ParentID2 = SlaveSidesArray(iQuad2Quad+1,DATA_ParentID)
  LocSide1  = SlaveSidesArray(iQuad2Quad+0,DATA_LocSide)
  LocSide2  = SlaveSidesArray(iQuad2Quad+1,DATA_LocSide)
  IF (TreeID1 .EQ. TreeID2) THEN
    IF (ParentID1 .EQ. ParentID2) THEN
      IF (LocSide1 .EQ. LocSide2) THEN
        Flag = .TRUE.
      END IF
    END IF
  END IF
  IF (Flag .EQV. .TRUE.) THEN
    BucketID = BucketID+1
    LocSide = LocSide1
    SideID_in(1:2)   = SlaveSidesArray(iQuad2Quad:iQuad2Quad+1,DATA_SideID)
    NodeID_in(1,1:4) = MeshData_FacesToNodes(1:4,SideID_in(1))
    NodeID_in(2,1:4) = MeshData_FacesToNodes(1:4,SideID_in(2))
    CALL CreateQuad2QuadNodes(LocSide,NodeID_in(1:2,1:4),NodeID_out(1:6))
    QuadData(1:6) = NodeID_out(1:6)
    QuadData(7:8) = (/iQuad2Quad,iQuad2Quad+1/)
    aQuadData%Data(1:SIZE(QuadData)) = QuadData
    CALL CreateLinkedListNode(aQuad2Quad,BucketID,aQuadData)
    CALL AddLinkedListNode(aQuad2Quad,Quad2QuadList)
  END IF
  iQuad2Quad = iQuad2Quad+2
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(Quad2QuadList,nQuad2Quad)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(Quad2QuadArray)) THEN
  DEALLOCATE(Quad2QuadArray)
END IF
ALLOCATE(Quad2QuadArray(1:nQuad2Quad,1:6+2))

! Exporting inner sides (non-conforming) data from list to array
iQuad2Quad = 0
aQuad2Quad => Quad2QuadList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aQuad2Quad))
  iQuad2Quad = iQuad2Quad+1
  CALL GetLinkedListNode(aQuad2Quad,BucketID,aQuadData)
  Quad2QuadArray(iQuad2Quad,1:6+2) = aQuadData%Data(1:6+2)
  aQuad2Quad => aQuad2Quad%NextLinkedListNode
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Quad2QuadArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  IF (nQuad2Quad .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in Quad2QuadArray ***"
  ELSE
    FormatString = "(1(2X,I8),8(2X,I8))"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "iQuad", "NodeIDs"
    DO iQuad2Quad=1,nQuad2Quad
      WRITE(UNIT_SCREEN,FormatString) &
        iQuad2Quad, &
        Quad2QuadArray(iQuad2Quad,1:6+2)
    END DO
  END IF
END IF

CALL DestructLinkedList(Quad2QuadList)

IF (nQuad2Quad .GT. 0) THEN
  IF (ALLOCATED(MasterSideAUX)) THEN
    DEALLOCATE(MasterSideAUX)
  END IF
  IF (ALLOCATED(Quad2QuadAUX)) THEN
    DEALLOCATE(Quad2QuadAUX)
  END IF
  ALLOCATE(MasterSideAUX(1:nQuad2Quad,1:4+1))
  ALLOCATE(Quad2QuadAUX(1:nQuad2Quad,1:4+1))
  DO iQuad2Quad=1,nQuad2Quad
    NodeID(1:4) = MeshData_FacesToNodes(1:4,MasterSidesArray(iQuad2Quad,DATA_SideID))
    CALL QuickSort(NodeID)
    MasterSideAUX(iQuad2Quad,1:4) = NodeID(1:4)
    MasterSideAUX(iQuad2Quad,5)   = iQuad2Quad
  END DO
  CALL QuickSortArray(MasterSideAUX,5)
  DO iQuad2Quad=1,nQuad2Quad
    NodeID(1:4) = Quad2QuadArray(iQuad2Quad,1:4)
    CALL QuickSort(NodeID)
    Quad2QuadAUX(iQuad2Quad,1:4) = NodeID(1:4)
    Quad2QuadAUX(iQuad2Quad,5)   = iQuad2Quad
  END DO
  CALL QuickSortArray(Quad2QuadAUX,5)
  IF (ALLOCATED(MeshData_MasterSlavesToNodes)) THEN
    DEALLOCATE(MeshData_MasterSlavesToNodes)
  END IF
  ALLOCATE(MeshData_MasterSlavesToNodes(1:6,1:nQuad2Quad))
  iQuad2Quad = 1
  DO WHILE (iQuad2Quad .LE. nQuad2Quad)
    Flag = .FALSE.
    IF (ALL(Quad2QuadAUX(iQuad2Quad,1:4) .EQ. MasterSideAUX(iQuad2Quad,1:4))) THEN
      Flag = .TRUE.
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      MeshData_MasterSlavesToNodes(1:6,iQuad2Quad) = Quad2QuadArray(iQuad2Quad,1:6)
    END IF
    iQuad2Quad = iQuad2Quad+1
  END DO
  IF (ALLOCATED(MeshData_MasterSlavesToElements)) THEN
    DEALLOCATE(MeshData_MasterSlavesToElements)
  END IF
  ALLOCATE(MeshData_MasterSlavesToElements(1:nQuad2Quad,1:2+1))
  iQuad2Quad = 1
  DO WHILE (iQuad2Quad .LE. nQuad2Quad)
    Flag = .FALSE.
    IF (ALL(Quad2QuadAUX(iQuad2Quad,1:4) .EQ. MasterSideAUX(iQuad2Quad,1:4))) THEN
      Flag = .TRUE.
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      MeshData_MasterSlavesToElements(iQuad2Quad,1) = MasterSidesArray(MasterSideAUX(iQuad2Quad,5),DATA_ElemID)
      MeshData_MasterSlavesToElements(iQuad2Quad,2) = SlaveSidesArray(Quad2QuadArray(Quad2QuadAUX(iQuad2Quad,5),7),DATA_ElemID)
      MeshData_MasterSlavesToElements(iQuad2Quad,3) = SlaveSidesArray(Quad2QuadArray(Quad2QuadAUX(iQuad2Quad,5),8),DATA_ElemID)
    END IF
    iQuad2Quad = iQuad2Quad+1
  END DO
  IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
    FormatString = "(4(2X,I8),1(2X,I8))"
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "MasterSideAUX"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "NodeIDs", "iQuad"
    DO iQuad2Quad=1,nQuad2Quad
      WRITE(UNIT_SCREEN,FormatString) MasterSideAUX(iQuad2Quad,1:5)
    END DO
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "Quad2QuadAUX"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "NodeIDs", "iQuad"
    DO iQuad2Quad=1,nQuad2Quad
      WRITE(UNIT_SCREEN,FormatString) Quad2QuadAUX(iQuad2Quad,1:5)
    END DO
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "Quad2QuadToNodes"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "iQuad", "NodeIDs"
    FormatString = "(1(2X,I8),6(2X,I8))"
    DO iQuad2Quad=1,nQuad2Quad
      WRITE(UNIT_SCREEN,FormatString) iQuad2Quad, MeshData_MasterSlavesToNodes(iQuad2Quad,1:6)
    END DO
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "Quad2QuadToElements"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "iQuad", "ElemIDs"
    FormatString = "(1(2X,I8),3(2X,I8))"
    DO iQuad2Quad=1,nQuad2Quad
      WRITE(UNIT_SCREEN,FormatString) iQuad2Quad, MeshData_MasterSlavesToElements(iQuad2Quad,1:3)
    END DO
  END IF
END IF

IF (ALLOCATED(NonConformingSidesArray)) THEN
  DEALLOCATE(NonConformingSidesArray)
END IF
IF (ALLOCATED(MasterSidesArray)) THEN
  DEALLOCATE(MasterSidesArray)
END IF
IF (ALLOCATED(SlaveSidesArray)) THEN
  DEALLOCATE(SlaveSidesArray)
END IF
IF (ALLOCATED(Quad2QuadArray)) THEN
  DEALLOCATE(Quad2QuadArray)
END IF
IF (ALLOCATED(MasterSideAUX)) THEN
  DEALLOCATE(MasterSideAUX)
END IF
IF (ALLOCATED(Quad2QuadAUX)) THEN
  DEALLOCATE(Quad2QuadAUX)
END IF
IF (ALLOCATED(HashTable)) THEN
  DEALLOCATE(HashTable)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetQuad2QuadArray
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetQuad4QuadArray(NonConformingSidesArray)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_FacesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToNodes
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tBucket
USE MOD_DataStructures,ONLY: GetBucketID
USE MOD_DataStructures,ONLY: CheckBucketIDIsInHashTable
USE MOD_DataStructures,ONLY: AddBucketIDToHashTable
USE MOD_DataStructures,ONLY: PrintHashTable
USE MOD_DataStructures,ONLY: CreateHashTable
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: tArrayINTEGER
USE MOD_DataStructures,ONLY: tLinkedList
USE MOD_DataStructures,ONLY: tLinkedListNode
USE MOD_DataStructures,ONLY: CreateLinkedListNode
USE MOD_DataStructures,ONLY: AddLinkedListNode
USE MOD_DataStructures,ONLY: GetLinkedListNode
USE MOD_DataStructures,ONLY: PrintLinkedList
USE MOD_DataStructures,ONLY: DestructLinkedList
USE MOD_DataStructures,ONLY: CountLinkedListNodes
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
INTEGER,ALLOCATABLE,INTENT(INOUT) :: NonConformingSidesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: DATA_ElemID
INTEGER :: DATA_TreeID
INTEGER :: DATA_ParentID
INTEGER :: DATA_Level
INTEGER :: DATA_LocChild
INTEGER :: DATA_LocSide
INTEGER :: DATA_SideID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nData
INTEGER :: BucketID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: TreeID1
INTEGER :: TreeID2
INTEGER :: TreeID3
INTEGER :: TreeID4
INTEGER :: ParentID1
INTEGER :: ParentID2
INTEGER :: ParentID3
INTEGER :: ParentID4
INTEGER :: LocChild1
INTEGER :: LocChild2
INTEGER :: LocChild3
INTEGER :: LocChild4
INTEGER :: LocSide1
INTEGER :: LocSide2
INTEGER :: LocSide3
INTEGER :: LocSide4
INTEGER :: LocSide
LOGICAL :: Flag
LOGICAL :: Flag1
LOGICAL :: Flag2
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iSide
INTEGER :: SideID
INTEGER :: iQuad4Quad
INTEGER :: nQuad4Quad
INTEGER :: iNonConformingSide
INTEGER :: nNonConformingSide
INTEGER :: NodeID(1:4)
INTEGER :: SideData(1:7)
INTEGER :: SideID_in(1:4)
INTEGER :: NodeID_in(1:4,1:4)
INTEGER :: NodeID_out(1:9)
INTEGER :: QuadData(1:9+4)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: MasterSideAUX(:,:)
INTEGER,ALLOCATABLE :: Quad4QuadAUX(:,:)
INTEGER,ALLOCATABLE :: Quad4QuadArray(:,:)
INTEGER,ALLOCATABLE :: MasterSidesArray(:,:)
INTEGER,ALLOCATABLE :: SlaveSidesArray(:,:)
!----------------------------------------------------------------------------------------------------------------------!
! ! ! TYPE(tHashTable)              :: HashTable
TYPE(tBucket),ALLOCATABLE     :: HashTable(:)
TYPE(tLinkedList)             :: SlaveSidesList
TYPE(tLinkedList)             :: MasterSidesList
TYPE(tLinkedList)             :: Quad4QuadList
TYPE(tArrayINTEGER)           :: aSideData
TYPE(tArrayINTEGER)           :: aQuadData
TYPE(tLinkedListNode),POINTER :: aQuad4Quad
TYPE(tLinkedListNode),POINTER :: aNonConformingSide
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

DATA_TreeID   = 1
DATA_ParentID = 2
DATA_Level    = 3
DATA_LocSide  = 4
DATA_LocChild = 5
DATA_ElemID   = 6
DATA_SideID   = 7

ALLOCATE(aQuadData%Data(1:SIZE(QuadData)))
ALLOCATE(aSideData%Data(1:SIZE(SideData)))

! Creating HashTable of non-conforming sides
nData = 4
nNonConformingSide = SIZE(NonConformingSidesArray,1)
CALL CreateHashTable(HashTable,nNonConformingSide,nData)

DO iNonConformingSide=1,nNonConformingSide
  SideID      = NonConformingSidesArray(iNonConformingSide,DATA_SideID)
  NodeID(1:4) = MeshData_FacesToNodes(1:4,SideID)
  CALL AddBucketIDToHashTable(HashTable,SideID,NodeID)
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "HashTable of Non-Conforming Sides"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  CALL PrintHashTable(HashTable)
END IF

! Creating MasterSidesList and SlaveSidesList
iNonConformingSide = 1
DO WHILE (iNonConformingSide .LE. nNonConformingSide)
  IF ((iNonConformingSide .GE. 1) .AND. (iNonConformingSide .LE. nNonConformingSide-3)) THEN
    Flag  = .FALSE.
    Flag1 = .FALSE.
    Flag2 = .FALSE.
    TreeID1   = NonConformingSidesArray(iNonConformingSide+0,DATA_TreeID)
    TreeID2   = NonConformingSidesArray(iNonConformingSide+1,DATA_TreeID)
    TreeID3   = NonConformingSidesArray(iNonConformingSide+2,DATA_TreeID)
    TreeID4   = NonConformingSidesArray(iNonConformingSide+3,DATA_TreeID)
    ParentID1 = NonConformingSidesArray(iNonConformingSide+0,DATA_ParentID)
    ParentID2 = NonConformingSidesArray(iNonConformingSide+1,DATA_ParentID)
    ParentID3 = NonConformingSidesArray(iNonConformingSide+2,DATA_ParentID)
    ParentID4 = NonConformingSidesArray(iNonConformingSide+3,DATA_ParentID)
    LocSide1  = NonConformingSidesArray(iNonConformingSide+0,DATA_LocSide)
    LocSide2  = NonConformingSidesArray(iNonConformingSide+1,DATA_LocSide)
    LocSide3  = NonConformingSidesArray(iNonConformingSide+2,DATA_LocSide)
    LocSide4  = NonConformingSidesArray(iNonConformingSide+3,DATA_LocSide)
    LocChild1 = NonConformingSidesArray(iNonConformingSide+0,DATA_LocChild)
    LocChild2 = NonConformingSidesArray(iNonConformingSide+1,DATA_LocChild)
    LocChild3 = NonConformingSidesArray(iNonConformingSide+2,DATA_LocChild)
    LocChild4 = NonConformingSidesArray(iNonConformingSide+3,DATA_LocChild)
    IF ((TreeID1 .EQ. TreeID2) .AND. (TreeID2 .EQ. TreeID3) .AND. (TreeID3 .EQ. TreeID4)) THEN
      IF ((ParentID1 .EQ. ParentID2) .AND. (ParentID2 .EQ. ParentID3) .AND. (ParentID3 .EQ. ParentID4)) THEN
        IF ((LocSide1 .EQ. LocSide2) .AND. (LocSide2 .EQ. LocSide3) .AND. (LocSide3 .EQ. LocSide4)) THEN
          SideID_in(1:4)   = NonConformingSidesArray(iNonConformingSide:iNonConformingSide+3,DATA_SideID)
          NodeID_in(1,1:4) = MeshData_FacesToNodes(1:4,SideID_in(1))
          NodeID_in(2,1:4) = MeshData_FacesToNodes(1:4,SideID_in(2))
          NodeID_in(3,1:4) = MeshData_FacesToNodes(1:4,SideID_in(3))
          NodeID_in(4,1:4) = MeshData_FacesToNodes(1:4,SideID_in(4))
          CALL CreateQuad4QuadNodes(LocSide1,NodeID_in(1:4,1:4),NodeID_out(1:9))
          Flag1 = CheckBucketIDIsInHashTable(HashTable,NodeID_out(1:4))
          Flag2 = CheckLocSideLocChild3D(LocSide1,(/LocChild1,LocChild2,LocChild3,LocChild4/))
          Flag  = (Flag1 .AND. Flag2)
        END IF
      END IF
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
    ELSE
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,MasterSidesList)
    END IF
    iNonConformingSide = iNonConformingSide+1
  ELSE
    Flag  = .FALSE.
    Flag1 = .FALSE.
    Flag2 = .FALSE.
    TreeID1   = NonConformingSidesArray(iNonConformingSide-3,DATA_TreeID)
    TreeID2   = NonConformingSidesArray(iNonConformingSide-2,DATA_TreeID)
    TreeID3   = NonConformingSidesArray(iNonConformingSide-1,DATA_TreeID)
    TreeID4   = NonConformingSidesArray(iNonConformingSide+0,DATA_TreeID)
    ParentID1 = NonConformingSidesArray(iNonConformingSide-3,DATA_ParentID)
    ParentID2 = NonConformingSidesArray(iNonConformingSide-2,DATA_ParentID)
    ParentID3 = NonConformingSidesArray(iNonConformingSide-1,DATA_ParentID)
    ParentID4 = NonConformingSidesArray(iNonConformingSide+0,DATA_ParentID)
    LocSide1  = NonConformingSidesArray(iNonConformingSide-3,DATA_LocSide)
    LocSide2  = NonConformingSidesArray(iNonConformingSide-2,DATA_LocSide)
    LocSide3  = NonConformingSidesArray(iNonConformingSide-1,DATA_LocSide)
    LocSide4  = NonConformingSidesArray(iNonConformingSide+0,DATA_LocSide)
    LocChild1 = NonConformingSidesArray(iNonConformingSide-3,DATA_LocChild)
    LocChild2 = NonConformingSidesArray(iNonConformingSide-2,DATA_LocChild)
    LocChild3 = NonConformingSidesArray(iNonConformingSide-1,DATA_LocChild)
    LocChild4 = NonConformingSidesArray(iNonConformingSide+0,DATA_LocChild)
    IF ((TreeID1 .EQ. TreeID2) .AND. (TreeID2 .EQ. TreeID3) .AND. (TreeID3 .EQ. TreeID4)) THEN
      IF ((ParentID1 .EQ. ParentID2) .AND. (ParentID2 .EQ. ParentID3) .AND. (ParentID3 .EQ. ParentID4)) THEN
        IF ((LocSide1 .EQ. LocSide2) .AND. (LocSide2 .EQ. LocSide3) .AND. (LocSide3 .EQ. LocSide4)) THEN
          SideID_in(1:4)   = NonConformingSidesArray(iNonConformingSide-3:iNonConformingSide,DATA_SideID)
          NodeID_in(1,1:4) = MeshData_FacesToNodes(1:4,SideID_in(1))
          NodeID_in(2,1:4) = MeshData_FacesToNodes(1:4,SideID_in(2))
          NodeID_in(3,1:4) = MeshData_FacesToNodes(1:4,SideID_in(3))
          NodeID_in(4,1:4) = MeshData_FacesToNodes(1:4,SideID_in(4))
          CALL CreateQuad4QuadNodes(LocSide1,NodeID_in(1:4,1:4),NodeID_out(1:9))
          Flag1 = CheckBucketIDIsInHashTable(HashTable,NodeID_out(1:4))
          Flag2 = CheckLocSideLocChild3D(LocSide1,(/LocChild1,LocChild2,LocChild3,LocChild4/))
          Flag  = (Flag1 .AND. Flag2)
        END IF
      END IF
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
      iNonConformingSide = iNonConformingSide+1
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,SlaveSidesList)
    ELSE
      SideData(1:7) = NonConformingSidesArray(iNonConformingSide,1:7)
      BucketID = iNonConformingSide
      aSideData%Data(1:SIZE(SideData)) = SideData
      CALL CreateLinkedListNode(aNonConformingSide,BucketID,aSideData)
      CALL AddLinkedListNode(aNonConformingSide,MasterSidesList)
    END IF
    iNonConformingSide = iNonConformingSide+1
  END IF
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(MasterSidesList,nNonConformingSide)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(MasterSidesArray)) THEN
  DEALLOCATE(MasterSidesArray)
END IF
ALLOCATE(MasterSidesArray(1:nNonConformingSide,1:7))

! Exporting inner sides (non-conforming) data from list to array
iNonConformingSide = 0
aNonConformingSide => MasterSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNonConformingSide))
  iNonConformingSide = iNonConformingSide+1
  CALL GetLinkedListNode(aNonConformingSide,BucketID,aSideData)
  MasterSidesArray(iNonConformingSide,1:7) = aSideData%Data(1:7)
  aNonConformingSide => aNonConformingSide%NextLinkedListNode
END DO

! Sorting array of inner sides (non-conforming)
CALL QuickSortArray(MasterSidesArray,5)

! Counting number of entries in list
CALL CountLinkedListNodes(SlaveSidesList,nNonConformingSide)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(SlaveSidesArray)) THEN
  DEALLOCATE(SlaveSidesArray)
END IF
ALLOCATE(SlaveSidesArray(1:nNonConformingSide,1:7))

! Exporting inner sides (non-conforming) data from list to array
iNonConformingSide = 0
aNonConformingSide => SlaveSidesList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aNonConformingSide))
  iNonConformingSide = iNonConformingSide+1
  CALL GetLinkedListNode(aNonConformingSide,BucketID,aSideData)
  SlaveSidesArray(iNonConformingSide,1:7) = aSideData%Data(1:7)
  aNonConformingSide => aNonConformingSide%NextLinkedListNode
END DO

! Sorting array of inner sides (non-conforming)
CALL QuickSortArray(SlaveSidesArray,5)

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "MasterSidesArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  FormatString = "(8(2X,I8),4(2X,I8))"
  WRITE(UNIT_SCREEN,"(9(2X,A8))") "iSide", "TreeID", "ParentID", "Level", "LocSide", "LocChild", "ElemID", "SideID", "NodeIDs"
  IF (SIZE(MasterSidesArray,1) .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in NonConformingSideArray ***"
  END IF
  DO iNonConformingSide=1,SIZE(MasterSidesArray,1)
    iSide = MasterSidesArray(iNonConformingSide,DATA_SideID)
    WRITE(UNIT_SCREEN,FormatString) &
      iNonConformingSide, &
      MasterSidesArray(iNonConformingSide,1:7), &
      MeshData_FacesToNodes(:,iSide)
  END DO
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "SlaveSidesArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  FormatString = "(8(2X,I8),4(2X,I8))"
  WRITE(UNIT_SCREEN,"(9(2X,A8))") "iSide", "TreeID", "ParentID", "Level", "LocSide", "LocChild", "ElemID", "SideID", "NodeIDs"
  IF (SIZE(SlaveSidesArray,1) .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in NonConformingSideArray ***"
  END IF
  DO iNonConformingSide=1,SIZE(SlaveSidesArray,1)
    iSide = SlaveSidesArray(iNonConformingSide,DATA_SideID)
    WRITE(UNIT_SCREEN,FormatString) &
      iNonConformingSide, &
      SlaveSidesArray(iNonConformingSide,1:7), &
      MeshData_FacesToNodes(:,iSide)
  END DO
END IF

CALL DestructLinkedList(MasterSidesList)
CALL DestructLinkedList(SlaveSidesList)

nQuad4Quad = SIZE(SlaveSidesArray,1)

! Removing repeated sides (only conforming sides)
BucketID = 0
iQuad4Quad = 1
DO WHILE (iQuad4Quad .LE. nQuad4Quad)
  Flag = .FALSE.
  TreeID1   = SlaveSidesArray(iQuad4Quad+0,DATA_TreeID)
  TreeID2   = SlaveSidesArray(iQuad4Quad+1,DATA_TreeID)
  TreeID3   = SlaveSidesArray(iQuad4Quad+2,DATA_TreeID)
  TreeID4   = SlaveSidesArray(iQuad4Quad+3,DATA_TreeID)
  ParentID1 = SlaveSidesArray(iQuad4Quad+0,DATA_ParentID)
  ParentID2 = SlaveSidesArray(iQuad4Quad+1,DATA_ParentID)
  ParentID3 = SlaveSidesArray(iQuad4Quad+2,DATA_ParentID)
  ParentID4 = SlaveSidesArray(iQuad4Quad+3,DATA_ParentID)
  LocSide1  = SlaveSidesArray(iQuad4Quad+0,DATA_LocSide)
  LocSide2  = SlaveSidesArray(iQuad4Quad+1,DATA_LocSide)
  LocSide3  = SlaveSidesArray(iQuad4Quad+2,DATA_LocSide)
  LocSide4  = SlaveSidesArray(iQuad4Quad+3,DATA_LocSide)
  IF ((TreeID1 .EQ. TreeID2) .AND. (TreeID2 .EQ. TreeID3) .AND. (TreeID3 .EQ. TreeID4)) THEN
    IF ((ParentID1 .EQ. ParentID2) .AND. (ParentID2 .EQ. ParentID3) .AND. (ParentID3 .EQ. ParentID4)) THEN
      IF ((LocSide1 .EQ. LocSide2) .AND. (LocSide2 .EQ. LocSide3) .AND. (LocSide3 .EQ. LocSide4)) THEN
        Flag = .TRUE.
      END IF
    END IF
  END IF
  IF (Flag .EQV. .TRUE.) THEN
    BucketID = BucketID+1
    LocSide = LocSide1
    SideID_in(1:4)   = SlaveSidesArray(iQuad4Quad:iQuad4Quad+3,DATA_SideID)
    NodeID_in(1,1:4) = MeshData_FacesToNodes(1:4,SideID_in(1))
    NodeID_in(2,1:4) = MeshData_FacesToNodes(1:4,SideID_in(2))
    NodeID_in(3,1:4) = MeshData_FacesToNodes(1:4,SideID_in(3))
    NodeID_in(4,1:4) = MeshData_FacesToNodes(1:4,SideID_in(4))
    CALL CreateQuad4QuadNodes(LocSide,NodeID_in(1:4,1:4),NodeID_out(1:9))
    QuadData(1:9)   = NodeID_out(1:9)
    QuadData(10:13) = (/iQuad4Quad,iQuad4Quad+1,iQuad4Quad+2,iQuad4Quad+3/)
    aQuadData%Data(1:SIZE(QuadData)) = QuadData
    CALL CreateLinkedListNode(aQuad4Quad,BucketID,aQuadData)
    CALL AddLinkedListNode(aQuad4Quad,Quad4QuadList)
  END IF
  iQuad4Quad = iQuad4Quad+4
END DO

! Counting number of entries in list
CALL CountLinkedListNodes(Quad4QuadList,nQuad4Quad)

! Allocating array of inner sides (non-conforming)
IF (ALLOCATED(Quad4QuadArray)) THEN
  DEALLOCATE(Quad4QuadArray)
END IF
ALLOCATE(Quad4QuadArray(1:nQuad4Quad,1:9+4))

! Exporting inner sides (non-conforming) data from list to array
iQuad4Quad = 0
aQuad4Quad => Quad4QuadList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aQuad4Quad))
  iQuad4Quad = iQuad4Quad+1
  CALL GetLinkedListNode(aQuad4Quad,BucketID,aQuadData)
  Quad4QuadArray(iQuad4Quad,1:9+4) = aQuadData%Data(1:9+4)
  aQuad4Quad => aQuad4Quad%NextLinkedListNode
END DO

IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  WRITE(UNIT_SCREEN,"(2X,A)") "Quad4QuadArray"
  WRITE(UNIT_SCREEN,"(2X,A)") "======================================================="
  IF (nQuad4Quad .EQ. 0) THEN
    WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in Quad4QuadArray ***"
  ELSE
    FormatString = "(1(2X,I8),13(2X,I8))"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "iQuad", "NodeIDs"
    DO iQuad4Quad=1,nQuad4Quad
      WRITE(UNIT_SCREEN,FormatString) &
        iQuad4Quad, &
        Quad4QuadArray(iQuad4Quad,1:9+4)
    END DO
  END IF
END IF

CALL DestructLinkedList(Quad4QuadList)

IF (nQuad4Quad .GT. 0) THEN
  IF (ALLOCATED(MasterSideAUX)) THEN
    DEALLOCATE(MasterSideAUX)
  END IF
  IF (ALLOCATED(Quad4QuadAUX)) THEN
    DEALLOCATE(Quad4QuadAUX)
  END IF
  ALLOCATE(MasterSideAUX(1:nQuad4Quad,1:4+1))
  ALLOCATE(Quad4QuadAUX(1:nQuad4Quad,1:4+1))
  DO iQuad4Quad=1,nQuad4Quad
    NodeID(1:4) = MeshData_FacesToNodes(1:4,MasterSidesArray(iQuad4Quad,DATA_SideID))
    CALL QuickSort(NodeID)
    MasterSideAUX(iQuad4Quad,1:4) = NodeID(1:4)
    MasterSideAUX(iQuad4Quad,5)   = iQuad4Quad
  END DO
  CALL QuickSortArray(MasterSideAUX,5)
  DO iQuad4Quad=1,nQuad4Quad
    NodeID(1:4) = Quad4QuadArray(iQuad4Quad,1:4)
    CALL QuickSort(NodeID)
    Quad4QuadAUX(iQuad4Quad,1:4) = NodeID(1:4)
    Quad4QuadAUX(iQuad4Quad,5)   = iQuad4Quad
  END DO
  CALL QuickSortArray(Quad4QuadAUX,5)
  IF (ALLOCATED(MeshData_MasterSlavesToNodes)) THEN
    DEALLOCATE(MeshData_MasterSlavesToNodes)
  END IF
  ALLOCATE(MeshData_MasterSlavesToNodes(1:9,1:nQuad4Quad))
  iQuad4Quad = 1
  DO WHILE (iQuad4Quad .LE. nQuad4Quad)
    Flag = .FALSE.
    IF (ALL(Quad4QuadAUX(iQuad4Quad,1:4) .EQ. MasterSideAUX(iQuad4Quad,1:4))) THEN
      Flag = .TRUE.
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      MeshData_MasterSlavesToNodes(1:9,iQuad4Quad) = Quad4QuadArray(iQuad4Quad,1:9)
    END IF
    iQuad4Quad = iQuad4Quad+1
  END DO
  IF (ALLOCATED(MeshData_MasterSlavesToElements)) THEN
    DEALLOCATE(MeshData_MasterSlavesToElements)
  END IF
  ALLOCATE(MeshData_MasterSlavesToElements(1:nQuad4Quad,1:4+1))
  iQuad4Quad = 1
  DO WHILE (iQuad4Quad .LE. nQuad4Quad)
    Flag = .FALSE.
    IF (ALL(Quad4QuadAUX(iQuad4Quad,1:4) .EQ. MasterSideAUX(iQuad4Quad,1:4))) THEN
      Flag = .TRUE.
    END IF
    IF (Flag .EQV. .TRUE.) THEN
      MeshData_MasterSlavesToElements(iQuad4Quad,1) = MasterSidesArray(MasterSideAUX(iQuad4Quad,5),DATA_ElemID)
      MeshData_MasterSlavesToElements(iQuad4Quad,2) = SlaveSidesArray(Quad4QuadArray(Quad4QuadAUX(iQuad4Quad,5),10),DATA_ElemID)
      MeshData_MasterSlavesToElements(iQuad4Quad,3) = SlaveSidesArray(Quad4QuadArray(Quad4QuadAUX(iQuad4Quad,5),11),DATA_ElemID)
      MeshData_MasterSlavesToElements(iQuad4Quad,4) = SlaveSidesArray(Quad4QuadArray(Quad4QuadAUX(iQuad4Quad,5),12),DATA_ElemID)
      MeshData_MasterSlavesToElements(iQuad4Quad,5) = SlaveSidesArray(Quad4QuadArray(Quad4QuadAUX(iQuad4Quad,5),13),DATA_ElemID)
    END IF
    iQuad4Quad = iQuad4Quad+1
  END DO
  IF (ParametersMeshElementsList%DebugMeshElementsList .EQV. .TRUE.) THEN
    FormatString = "(4(2X,I8),1(2X,I8))"
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "MasterSideAUX"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "NodeIDs", "iQuad"
    DO iQuad4Quad=1,nQuad4Quad
      WRITE(UNIT_SCREEN,FormatString) MasterSideAUX(iQuad4Quad,1:5)
    END DO
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "Quad4QuadAUX"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "NodeIDs", "iQuad"
    DO iQuad4Quad=1,nQuad4Quad
      WRITE(UNIT_SCREEN,FormatString) Quad4QuadAUX(iQuad4Quad,1:5)
    END DO
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "Quad4QuadToNodes"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "iQuad", "NodeIDs"
    FormatString = "(1(2X,I8),9(2X,I8))"
    DO iQuad4Quad=1,nQuad4Quad
      WRITE(UNIT_SCREEN,FormatString) iQuad4Quad, MeshData_MasterSlavesToNodes(iQuad4Quad,1:9)
    END DO
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,*)
    WRITE(UNIT_SCREEN,"(1(2X,A))") "Quad4QuadToElements"
    WRITE(UNIT_SCREEN,"(2(2X,A8))") "iQuad", "ElemIDs"
    FormatString = "(1(2X,I8),5(2X,I8))"
    DO iQuad4Quad=1,nQuad4Quad
      WRITE(UNIT_SCREEN,FormatString) iQuad4Quad, MeshData_MasterSlavesToElements(iQuad4Quad,1:5)
    END DO
  END IF
END IF

IF (ALLOCATED(NonConformingSidesArray)) THEN
  DEALLOCATE(NonConformingSidesArray)
END IF
IF (ALLOCATED(MasterSidesArray)) THEN
  DEALLOCATE(MasterSidesArray)
END IF
IF (ALLOCATED(SlaveSidesArray)) THEN
  DEALLOCATE(SlaveSidesArray)
END IF
IF (ALLOCATED(Quad4QuadArray)) THEN
  DEALLOCATE(Quad4QuadArray)
END IF
IF (ALLOCATED(MasterSideAUX)) THEN
  DEALLOCATE(MasterSideAUX)
END IF
IF (ALLOCATED(Quad4QuadAUX)) THEN
  DEALLOCATE(Quad4QuadAUX)
END IF
IF (ALLOCATED(HashTable)) THEN
  DEALLOCATE(HashTable)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetQuad4QuadArray
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateQuad2QuadNodes(LocSide,NodeID_in,NodeID_out)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: LocSide
INTEGER,INTENT(IN)  :: NodeID_in(1:2,1:4)
INTEGER,INTENT(OUT) :: NodeID_out(1:6)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: Nodes(1:6)
INTEGER :: Children(1:6,1:6)
!----------------------------------------------------------------------------------------------------------------------!

Nodes = (/1,2,3,4,2,4/)

Children(1,1:6) = (/1,2,2,1,1,2/)
Children(2,1:6) = (/1,2,2,1,1,2/)
Children(3,1:6) = (/1,2,2,1,1,2/)
Children(4,1:6) = (/2,1,1,2,2,1/)
Children(5,1:6) = (/1,2,2,1,2,1/)
Children(6,1:6) = (/1,2,2,1,1,2/)

DO iNode=1,6
  NodeID_out(iNode) = NodeID_in(Children(LocSide,iNode),Nodes(iNode))
END DO

Nodes = (/4,1,2,3,4,2/)
Children(5,1:6) = (/2,1,1,2,1,2/)

IF (LocSide .EQ. 5) THEN
  DO iNode=1,6
    NodeID_out(iNode) = NodeID_in(Children(LocSide,iNode),Nodes(iNode))
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateQuad2QuadNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateQuad4QuadNodes(LocSide,NodeID_in,NodeID_out)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)  :: LocSide
INTEGER,INTENT(IN)  :: NodeID_in(1:4,1:4)
INTEGER,INTENT(OUT) :: NodeID_out(1:9)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: Nodes(1:9)
INTEGER :: Children(1:6,1:9)
!----------------------------------------------------------------------------------------------------------------------!

Nodes = (/1,2,3,4,2,3,4,1,3/)

Children(1,1:9) = (/1,3,4,2,1,3,4,2,1/)
Children(2,1:9) = (/1,2,4,3,1,2,4,3,1/)
Children(3,1:9) = (/1,2,4,3,1,2,4,3,1/)
Children(4,1:9) = (/2,1,3,4,2,1,3,4,2/)
Children(5,1:9) = (/1,3,4,2,1,3,4,2,1/)
Children(6,1:9) = (/1,2,4,3,1,2,4,3,1/)

DO iNode=1,9
  NodeID_out(iNode) = NodeID_in(Children(LocSide,iNode),Nodes(iNode))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateQuad4QuadNodes
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CheckLocSideLocChild3D(LocSide,LocChild) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: LocSide
INTEGER,INTENT(IN) :: LocChild(1:4)
LOGICAL            :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: SortedLocChild(1:4)
INTEGER :: ChildrenByLocSide(1:6,1:4)
!----------------------------------------------------------------------------------------------------------------------!

IF (ALL(LocChild(1:4) .EQ. 0) .EQV. .TRUE.) THEN
  Flag = .FALSE.
  RETURN
END IF

ChildrenByLocSide(1,1:4) = (/1,2,3,4/)
ChildrenByLocSide(2,1:4) = (/1,2,5,6/)
ChildrenByLocSide(3,1:4) = (/2,4,6,8/)
ChildrenByLocSide(4,1:4) = (/3,4,7,8/)
ChildrenByLocSide(5,1:4) = (/1,3,5,7/)
ChildrenByLocSide(6,1:4) = (/5,6,7,8/)

SortedLocChild(1:4) = LocChild(1:4)
CALL QuickSort(SortedLocChild)

IF (ALL(ChildrenByLocSide(LocSide,1:4) .EQ. SortedLocChild(1:4))) THEN
  Flag = .TRUE.
ELSE
  Flag = .FALSE.
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CheckLocSideLocChild3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CheckLocSideLocChild3D_XY(LocSide,LocChild) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: QuickSort
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: LocSide
INTEGER,INTENT(IN) :: LocChild(1:2)
LOGICAL            :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: SortedLocChild(1:2)
INTEGER :: ChildrenByLocSide(1:6,1:2)
!----------------------------------------------------------------------------------------------------------------------!

IF (ALL(LocChild(1:2) .EQ. 0) .EQV. .TRUE.) THEN
  Flag = .FALSE.
  RETURN
END IF

ChildrenByLocSide(1,1:2) = (/0,0/)
ChildrenByLocSide(2,1:2) = (/1,2/)
ChildrenByLocSide(3,1:2) = (/2,4/)
ChildrenByLocSide(4,1:2) = (/3,4/)
ChildrenByLocSide(5,1:2) = (/1,3/)
ChildrenByLocSide(6,1:2) = (/0,0/)

SortedLocChild(1:2) = LocChild(1:2)
CALL QuickSort(SortedLocChild)

IF (ALL(ChildrenByLocSide(LocSide,1:2) .EQ. SortedLocChild(1:2))) THEN
  Flag = .TRUE.
ELSE
  Flag = .FALSE.
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CheckLocSideLocChild3D_XY
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinementConnectivity
!======================================================================================================================!
