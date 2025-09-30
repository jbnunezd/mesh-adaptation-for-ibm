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
MODULE MOD_MeshRefinementSplitting
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE RefineElements
  MODULE PROCEDURE RefineElements
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: RefineElements
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
SUBROUTINE RefineElements(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL RefineElements3D(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RefineElements
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RefineElements3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: IsotropicRefinement
USE MOD_MeshRefinement_vars,ONLY: WhichRefinementPlane
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshElementsList,ONLY: SetUniqueNodes
USE MOD_MeshElementsList,ONLY: SetUniqueElemID
USE MOD_MeshElementsList,ONLY: SetUniqueSideID
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  IF (MeshData_ElementsToRefineFlag(ElemID) .EQV. .TRUE.) THEN
    IF (IsotropicRefinement .EQV. .FALSE.) THEN
      SELECT CASE(LowerCase(WhichRefinementPlane))
        CASE("z")
          CALL SplitHexa_XY(aElem)
        CASE DEFAULT
        ErrorMessage = "RefineElements3D: No-Refine-Direction not implemented"
        CALL PrintError(__STAMP__,ErrorMessage)
      END SELECT
    ELSE
      CALL SplitHexa(aElem)
    END IF
  END IF
  aElem => aElem%NextElem
END DO

MeshInfo%MaxRefLevel = Level

CALL SetUniqueNodes()
CALL SetUniqueElemID()
CALL SetUniqueSideID()

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RefineElements3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitHexa(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!ç
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateHexaElem
!----------------------------------------------------------------------------------------------------------------------!ç
USE MOD_MeshRefinement_vars,ONLY: RefineMeshAroundGeometry
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: WhichGeometryDistribution
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometryDistribution,ONLY: DistributeSTLFacetsInElem
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: mIni = 0 
INTEGER,PARAMETER :: mMid = 1
INTEGER,PARAMETER :: mEnd = 2
INTEGER,PARAMETER :: nFaces = 6
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii, jj, kk
INTEGER :: LocSideID
INTEGER :: LocChild
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(mIni:mEnd-1,mIni:mEnd-1,mIni:mEnd-1)
TYPE(tNodePtr) :: NewNodes(mIni:mEnd,mIni:mEnd,mIni:mEnd)
TYPE(tSidePtr) :: NewSides(mIni:mEnd-1,mIni:mEnd-1,mIni:mEnd-1,1:nFaces)
TYPE(tNodePtr) :: CurvedNode(0:1,0:1,0:1)
!----------------------------------------------------------------------------------------------------------------------!

dh = 0.5
NewNodes(mIni,mIni,mIni)%Node => Elem%Nodes(1)%Node
NewNodes(mEnd,mIni,mIni)%Node => Elem%Nodes(2)%Node
NewNodes(mEnd,mEnd,mIni)%Node => Elem%Nodes(3)%Node
NewNodes(mIni,mEnd,mIni)%Node => Elem%Nodes(4)%Node
NewNodes(mIni,mIni,mEnd)%Node => Elem%Nodes(5)%Node
NewNodes(mEnd,mIni,mEnd)%Node => Elem%Nodes(6)%Node
NewNodes(mEnd,mEnd,mEnd)%Node => Elem%Nodes(7)%Node
NewNodes(mIni,mEnd,mEnd)%Node => Elem%Nodes(8)%Node

! New midnodes at edges
CALL CreateNode(NewNodes(mMid,mIni,mIni)%Node)
CALL CreateNode(NewNodes(mIni,mMid,mIni)%Node)
CALL CreateNode(NewNodes(mMid,mEnd,mIni)%Node)
CALL CreateNode(NewNodes(mEnd,mMid,mIni)%Node)
NewNodes(mMid,mIni,mIni)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mEnd,mIni,mIni)%Node%Coords)
NewNodes(mIni,mMid,mIni)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mIni,mEnd,mIni)%Node%Coords)
NewNodes(mMid,mEnd,mIni)%Node%Coords = dh*(NewNodes(mIni,mEnd,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mIni)%Node%Coords)
NewNodes(mEnd,mMid,mIni)%Node%Coords = dh*(NewNodes(mEnd,mIni,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mIni)%Node%Coords)

CALL CreateNode(NewNodes(mMid,mIni,mEnd)%Node)
CALL CreateNode(NewNodes(mIni,mMid,mEnd)%Node)
CALL CreateNode(NewNodes(mMid,mEnd,mEnd)%Node)
CALL CreateNode(NewNodes(mEnd,mMid,mEnd)%Node)
NewNodes(mMid,mIni,mEnd)%Node%Coords = dh*(NewNodes(mIni,mIni,mEnd)%Node%Coords + NewNodes(mEnd,mIni,mEnd)%Node%Coords)
NewNodes(mIni,mMid,mEnd)%Node%Coords = dh*(NewNodes(mIni,mIni,mEnd)%Node%Coords + NewNodes(mIni,mEnd,mEnd)%Node%Coords)
NewNodes(mMid,mEnd,mEnd)%Node%Coords = dh*(NewNodes(mIni,mEnd,mEnd)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)
NewNodes(mEnd,mMid,mEnd)%Node%Coords = dh*(NewNodes(mEnd,mIni,mEnd)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)

CALL CreateNode(NewNodes(mIni,mIni,mMid)%Node)
CALL CreateNode(NewNodes(mEnd,mIni,mMid)%Node)
CALL CreateNode(NewNodes(mEnd,mEnd,mMid)%Node)
CALL CreateNode(NewNodes(mIni,mEnd,mMid)%Node)
NewNodes(mIni,mIni,mMid)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mIni,mIni,mEnd)%Node%Coords)
NewNodes(mEnd,mIni,mMid)%Node%Coords = dh*(NewNodes(mEnd,mIni,mIni)%Node%Coords + NewNodes(mEnd,mIni,mEnd)%Node%Coords)
NewNodes(mEnd,mEnd,mMid)%Node%Coords = dh*(NewNodes(mEnd,mEnd,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)
NewNodes(mIni,mEnd,mMid)%Node%Coords = dh*(NewNodes(mIni,mEnd,mIni)%Node%Coords + NewNodes(mIni,mEnd,mEnd)%Node%Coords)

! New midnodes at faces
CALL CreateNode(NewNodes(mMid,mMid,mIni)%Node)
CALL CreateNode(NewNodes(mMid,mIni,mMid)%Node)
CALL CreateNode(NewNodes(mEnd,mMid,mMid)%Node)
CALL CreateNode(NewNodes(mMid,mEnd,mMid)%Node)
CALL CreateNode(NewNodes(mIni,mMid,mMid)%Node)
CALL CreateNode(NewNodes(mMid,mMid,mEnd)%Node)
NewNodes(mMid,mMid,mIni)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mIni)%Node%Coords)
NewNodes(mMid,mIni,mMid)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mEnd,mIni,mEnd)%Node%Coords)
NewNodes(mEnd,mMid,mMid)%Node%Coords = dh*(NewNodes(mEnd,mIni,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)
NewNodes(mMid,mEnd,mMid)%Node%Coords = dh*(NewNodes(mIni,mEnd,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)
NewNodes(mIni,mMid,mMid)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mIni,mEnd,mEnd)%Node%Coords)
NewNodes(mMid,mMid,mEnd)%Node%Coords = dh*(NewNodes(mIni,mIni,mEnd)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)

! New node at center of element
CALL CreateNode(NewNodes(mMid,mMid,mMid)%Node)
NewNodes(mMid,mMid,mMid)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)

LocChild = 0
DO kk=0,1
  DO jj=0,1
    DO ii=0,1
      LocChild = LocChild+1
      CurvedNode(0,0,0)%Node => NewNodes(ii+0,jj+0,kk+0)%Node
      CurvedNode(1,0,0)%Node => NewNodes(ii+1,jj+0,kk+0)%Node
      CurvedNode(1,1,0)%Node => NewNodes(ii+1,jj+1,kk+0)%Node
      CurvedNode(0,1,0)%Node => NewNodes(ii+0,jj+1,kk+0)%Node
      CurvedNode(0,0,1)%Node => NewNodes(ii+0,jj+0,kk+1)%Node
      CurvedNode(1,0,1)%Node => NewNodes(ii+1,jj+0,kk+1)%Node
      CurvedNode(1,1,1)%Node => NewNodes(ii+1,jj+1,kk+1)%Node
      CurvedNode(0,1,1)%Node => NewNodes(ii+0,jj+1,kk+1)%Node
      CALL CreateHexaElem(NewElems(ii,jj,kk)%Elem,1,CurvedNode)
      NewElems(ii,jj,kk)%Elem%Flag        = Elem%Flag
      NewElems(ii,jj,kk)%Elem%TreeID      = Elem%TreeID
      NewElems(ii,jj,kk)%Elem%Level       = Elem%Level+1
      NewElems(ii,jj,kk)%Elem%LocChild    = LocChild
      NewElems(ii,jj,kk)%Elem%Coords(1:3) = (/ii,jj,kk/) + 2*Elem%Coords(1:3)
      IF (Elem%Level .GE. 1) THEN
        NewElems(ii,jj,kk)%Elem%ParentID = GetParentID(Elem%Coords(1:3),Elem%Level)
      ELSE
        NewElems(ii,jj,kk)%Elem%ParentID = Elem%TreeID
      END IF
      !***********************************!
      IF (RefineMeshAroundGeometry .EQV. .TRUE.) THEN
        SELECT CASE(LowerCase(WhichGeometryDistribution))
          CASE("stl-facets-distribution")
            IF (Elem%nFacets .GT. 0) THEN
              CALL DistributeSTLFacetsInElem(NewElems(ii,jj,kk)%Elem,Elem%Facets)
            END IF
        END SELECT
      END IF
      !***********************************!
      LocSideID = 0
      aSide => NewElems(ii,jj,kk)%Elem%FirstSide
      DO WHILE (ASSOCIATED(aSide))
        LocSideID = LocSideID+1
        NewSides(ii,jj,kk,LocSideID)%Side => aSide
        aSide => aSide%NextElemSide
      END DO
    END DO
  END DO
END DO

LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO jj=0,1
      DO ii=0,1
        SELECT CASE (LocSideID)
          CASE(1) ! Z-
            CALL CopyBC(aSide,NewSides(ii,jj,0,LocSideID)%Side)
          CASE(2) ! Y-
            CALL CopyBC(aSide,NewSides(ii,0,jj,LocSideID)%Side)
          CASE(3) ! X+
            CALL CopyBC(aSide,NewSides(1,ii,jj,LocSideID)%Side)
          CASE(4) ! Y+
            CALL CopyBC(aSide,NewSides(ii,1,jj,LocSideID)%Side)
          CASE(5) ! X-
            CALL CopyBC(aSide,NewSides(0,ii,jj,LocSideID)%Side)
          CASE(6) ! Z+
            CALL CopyBC(aSide,NewSides(ii,jj,1,LocSideID)%Side)
        END SELECT
      END DO
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem
DO kk=0,1
  DO jj=0,1
    DO ii=0,1
      aElem%NextElem          => NewElems(ii,jj,kk)%Elem
      aElem%NextElem%PrevElem => aElem
      aElem                   => aElem%NextElem
    END DO
  END DO
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(0,0,0)%Elem%PrevElem => PrevElem
  PrevElem%NextElem             => NewElems(0,0,0)%Elem
ELSE
  ElemList%FirstElem => NewElems(0,0,0)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitHexa
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitHexa_XY(Elem)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!ç
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: tSide
USE MOD_MeshMain_vars,ONLY: tElemPtr
USE MOD_MeshMain_vars,ONLY: tNodePtr
USE MOD_MeshMain_vars,ONLY: tSidePtr
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CopyBC
USE MOD_MeshMainMethods,ONLY: RemoveElem
USE MOD_MeshMainMethods,ONLY: CreateNode
USE MOD_MeshMainMethods,ONLY: CreateHexaElem
!----------------------------------------------------------------------------------------------------------------------!ç
USE MOD_MeshRefinement_vars,ONLY: RefineMeshAroundGeometry
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: WhichGeometryDistribution
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometryDistribution,ONLY: DistributeSTLFacetsInElem
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT) :: Elem
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: mIni = 0 
INTEGER,PARAMETER :: mMid = 1
INTEGER,PARAMETER :: mEnd = 2
INTEGER,PARAMETER :: nFaces = 6
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii, jj, kk
INTEGER :: LocSideID
INTEGER :: LocChild
REAL    :: dh
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
TYPE(tElem),POINTER :: PrevElem
TYPE(tElem),POINTER :: NextElem
TYPE(tSide),POINTER :: aSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemPtr) :: NewElems(mIni:mEnd-1,mIni:mEnd-1,mIni:mEnd-1)
TYPE(tNodePtr) :: NewNodes(mIni:mEnd,mIni:mEnd,mIni:mEnd)
TYPE(tSidePtr) :: NewSides(mIni:mEnd-1,mIni:mEnd-1,mIni:mEnd-1,1:nFaces)
TYPE(tNodePtr) :: CurvedNode(0:1,0:1,0:1)
!----------------------------------------------------------------------------------------------------------------------!

dh = 0.5
NewNodes(mIni,mIni,mIni)%Node => Elem%Nodes(1)%Node
NewNodes(mEnd,mIni,mIni)%Node => Elem%Nodes(2)%Node
NewNodes(mEnd,mEnd,mIni)%Node => Elem%Nodes(3)%Node
NewNodes(mIni,mEnd,mIni)%Node => Elem%Nodes(4)%Node
NewNodes(mIni,mIni,mEnd)%Node => Elem%Nodes(5)%Node
NewNodes(mEnd,mIni,mEnd)%Node => Elem%Nodes(6)%Node
NewNodes(mEnd,mEnd,mEnd)%Node => Elem%Nodes(7)%Node
NewNodes(mIni,mEnd,mEnd)%Node => Elem%Nodes(8)%Node

! New midnodes at edges, Z- boundary plane
CALL CreateNode(NewNodes(mMid,mIni,mIni)%Node)
CALL CreateNode(NewNodes(mIni,mMid,mIni)%Node)
CALL CreateNode(NewNodes(mMid,mEnd,mIni)%Node)
CALL CreateNode(NewNodes(mEnd,mMid,mIni)%Node)
NewNodes(mMid,mIni,mIni)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mEnd,mIni,mIni)%Node%Coords)
NewNodes(mIni,mMid,mIni)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mIni,mEnd,mIni)%Node%Coords)
NewNodes(mMid,mEnd,mIni)%Node%Coords = dh*(NewNodes(mIni,mEnd,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mIni)%Node%Coords)
NewNodes(mEnd,mMid,mIni)%Node%Coords = dh*(NewNodes(mEnd,mIni,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mIni)%Node%Coords)

! New midnodes at edges, Z+ boundary plane
CALL CreateNode(NewNodes(mMid,mIni,mEnd)%Node)
CALL CreateNode(NewNodes(mIni,mMid,mEnd)%Node)
CALL CreateNode(NewNodes(mMid,mEnd,mEnd)%Node)
CALL CreateNode(NewNodes(mEnd,mMid,mEnd)%Node)
NewNodes(mMid,mIni,mEnd)%Node%Coords = dh*(NewNodes(mIni,mIni,mEnd)%Node%Coords + NewNodes(mEnd,mIni,mEnd)%Node%Coords)
NewNodes(mIni,mMid,mEnd)%Node%Coords = dh*(NewNodes(mIni,mIni,mEnd)%Node%Coords + NewNodes(mIni,mEnd,mEnd)%Node%Coords)
NewNodes(mMid,mEnd,mEnd)%Node%Coords = dh*(NewNodes(mIni,mEnd,mEnd)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)
NewNodes(mEnd,mMid,mEnd)%Node%Coords = dh*(NewNodes(mEnd,mIni,mEnd)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)

! New midnodes at faces
CALL CreateNode(NewNodes(mMid,mMid,mIni)%Node)
CALL CreateNode(NewNodes(mMid,mMid,mEnd)%Node)
NewNodes(mMid,mMid,mIni)%Node%Coords = dh*(NewNodes(mIni,mIni,mIni)%Node%Coords + NewNodes(mEnd,mEnd,mIni)%Node%Coords)
NewNodes(mMid,mMid,mEnd)%Node%Coords = dh*(NewNodes(mIni,mIni,mEnd)%Node%Coords + NewNodes(mEnd,mEnd,mEnd)%Node%Coords)

LocChild = 0
DO jj=0,1
  DO ii=0,1
    kk = 0
    LocChild = LocChild+1
    CurvedNode(0,0,0)%Node => NewNodes(ii+0,jj+0,mIni)%Node
    CurvedNode(1,0,0)%Node => NewNodes(ii+1,jj+0,mIni)%Node
    CurvedNode(1,1,0)%Node => NewNodes(ii+1,jj+1,mIni)%Node
    CurvedNode(0,1,0)%Node => NewNodes(ii+0,jj+1,mIni)%Node
    CurvedNode(0,0,1)%Node => NewNodes(ii+0,jj+0,mEnd)%Node
    CurvedNode(1,0,1)%Node => NewNodes(ii+1,jj+0,mEnd)%Node
    CurvedNode(1,1,1)%Node => NewNodes(ii+1,jj+1,mEnd)%Node
    CurvedNode(0,1,1)%Node => NewNodes(ii+0,jj+1,mEnd)%Node
    CALL CreateHexaElem(NewElems(ii,jj,kk)%Elem,1,CurvedNode)
    NewElems(ii,jj,kk)%Elem%Flag        = Elem%Flag
    NewElems(ii,jj,kk)%Elem%TreeID      = Elem%TreeID
    NewElems(ii,jj,kk)%Elem%Level       = Elem%Level+1
    NewElems(ii,jj,kk)%Elem%LocChild    = LocChild
    NewElems(ii,jj,kk)%Elem%Coords(1:3) = (/ii,jj,kk/) + 2*Elem%Coords(1:3)
    IF (Elem%Level .GE. 1) THEN
      NewElems(ii,jj,kk)%Elem%ParentID = GetParentID(Elem%Coords(1:3),Elem%Level)
    ELSE
      NewElems(ii,jj,kk)%Elem%ParentID = Elem%TreeID
    END IF
    !***********************************!
    IF (RefineMeshAroundGeometry .EQV. .TRUE.) THEN
      SELECT CASE(LowerCase(WhichGeometryDistribution))
        CASE("stl-facets-distribution")
          IF (Elem%nFacets .GT. 0) THEN
            CALL DistributeSTLFacetsInElem(NewElems(ii,jj,kk)%Elem,Elem%Facets)
          END IF
      END SELECT
    END IF
    !***********************************!
    LocSideID = 0
    aSide => NewElems(ii,jj,kk)%Elem%FirstSide
    DO WHILE (ASSOCIATED(aSide))
      LocSideID = LocSideID+1
      NewSides(ii,jj,kk,LocSideID)%Side => aSide
      aSide => aSide%NextElemSide
    END DO
  END DO
END DO

LocSideID = 0
aSide => Elem%FirstSide
DO WHILE (ASSOCIATED(aSide))
  LocSideID = LocSideID+1
  IF (ASSOCIATED(aSide%BC)) THEN
    DO jj=0,1
      DO ii=0,1
        SELECT CASE (LocSideID)
          CASE(1) ! Z-
            CALL CopyBC(aSide,NewSides(ii,jj,0,LocSideID)%Side)
          CASE(2) ! Y-
            CALL CopyBC(aSide,NewSides(ii,0,0,LocSideID)%Side)
          CASE(3) ! X+
            CALL CopyBC(aSide,NewSides(1,ii,0,LocSideID)%Side)
          CASE(4) ! Y+
            CALL CopyBC(aSide,NewSides(ii,1,0,LocSideID)%Side)
          CASE(5) ! X-
            CALL CopyBC(aSide,NewSides(0,ii,0,LocSideID)%Side)
          CASE(6) ! Z+
            CALL CopyBC(aSide,NewSides(ii,jj,0,LocSideID)%Side)
        END SELECT
      END DO
    END DO
  END IF
  aSide => aSide%NextElemSide
END DO

aElem    => Elem
NextElem => Elem%NextElem
PrevElem => Elem%PrevElem
DO jj=0,1
  DO ii=0,1
    kk = 0
    aElem%NextElem          => NewElems(ii,jj,kk)%Elem
    aElem%NextElem%PrevElem => aElem
    aElem                   => aElem%NextElem
  END DO
END DO

IF (ASSOCIATED(NextElem)) THEN
  aElem%NextElem    => NextElem
  NextElem%PrevElem => aElem
END IF

CALL RemoveElem(Elem,ElemList)

Elem => aElem

IF (ASSOCIATED(PrevElem)) THEN
  NewElems(0,0,0)%Elem%PrevElem => PrevElem
  PrevElem%NextElem             => NewElems(0,0,0)%Elem
ELSE
  ElemList%FirstElem => NewElems(0,0,0)%Elem
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitHexa_XY
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetParentID(ParentCoords,Level) RESULT(ParentID)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_nDims
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: ParentCoords(1:PP_nDims)
INTEGER,INTENT(IN) :: Level 
INTEGER            :: ParentID
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLevel
INTEGER :: IniParentID
!----------------------------------------------------------------------------------------------------------------------!

IniParentID = 0
DO iLevel=1,Level
  IniParentID = IniParentID + 2**(PP_nDims*(iLevel-1))
END DO

ParentID = IniParentID + ParentCoords(1) + (2**Level)*ParentCoords(2) + (2**(2*Level))*ParentCoords(3)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetParentID
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinementSplitting
!======================================================================================================================!
