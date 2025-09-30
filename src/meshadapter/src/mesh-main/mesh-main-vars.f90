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
MODULE MOD_MeshMain_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tPointCoords
USE MOD_GeometryMain_vars,ONLY: tFacetCoords
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: PP_nDims
INTEGER :: PP_nElems
INTEGER :: PP_nNodes
INTEGER :: PP_N
INTEGER :: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
TYPE tMeshInfo
  INTEGER            :: nDims
  INTEGER            :: NGeo
  INTEGER            :: nElems
  INTEGER            :: nNodes
  INTEGER            :: nFaces
  INTEGER            :: nBCFaces
  INTEGER            :: nOutVars
  INTEGER            :: MaxRefLevel
  CHARACTER(LEN=256) :: FileVersion
  CHARACTER(LEN=256) :: CoordNames3D(1:3)
  CHARACTER(LEN=256) :: DataNames3D(1:2)
  CHARACTER(LEN=256) :: ProgramName
  CHARACTER(LEN=256) :: ProjectName
  CHARACTER(LEN=256) :: BaseFileName
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tMeshArraysInfo
  INTEGER :: nDims
  INTEGER :: NGeo
  INTEGER :: nElems
  INTEGER :: nNodes
  INTEGER :: nFaces
  INTEGER :: nBCFaces
  INTEGER :: nEdge2Edge
  INTEGER :: nQuad4Quad
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE tNode
  REAL    :: Coords(1:3)
  INTEGER :: NodeID
  INTEGER :: BCFlag
  INTEGER :: RefCount
  INTEGER :: tmp
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tNodePtr
  TYPE(tNode),POINTER :: Node
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tElem
  INTEGER :: nNodes
  INTEGER :: ElemID
  INTEGER :: Flag
  !=====================================!
  ! QUADTREES/OCTREES
  !=====================================!
  INTEGER :: TreeID
  INTEGER :: ParentID
  INTEGER :: Level
  INTEGER :: LocChild
  INTEGER :: nFacets
  INTEGER :: nPoints
  INTEGER,ALLOCATABLE :: Coords(:)
  !=====================================!
  TYPE(tFacetCoords),ALLOCATABLE :: Facets(:)
  TYPE(tPointCoords),ALLOCATABLE :: Points(:)
  !=====================================!
  TYPE(tElem),POINTER    :: PrevElem
  TYPE(tElem),POINTER    :: NextElem
  TYPE(tSide),POINTER    :: FirstSide
  TYPE(tNodePtr),POINTER :: Nodes(:)
END TYPE tElem
!----------------------------------------------------------------------------------------------------------------------!
TYPE tElemList
  TYPE(tElem),POINTER :: FirstElem => NULL()
  TYPE(tElem),POINTER :: LastElem  => NULL()
END TYPE tElemList
!----------------------------------------------------------------------------------------------------------------------!
TYPE tElemPtr
  TYPE(tElem),POINTER :: Elem
END TYPE tElemPtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tSide
  INTEGER                :: nNodes
  INTEGER                :: LocSide
  INTEGER                :: SideID
  TYPE(tNodePtr),POINTER :: Nodes(:)
  TYPE(tBC),POINTER      :: BC
  TYPE(tSide),POINTER    :: NextElemSide
END TYPE tSide
!----------------------------------------------------------------------------------------------------------------------!
TYPE tSidePtr
  TYPE(tSide),POINTER :: Side
END TYPE tSidePtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFaceNodes
  INTEGER                  :: FaceID
  INTEGER,ALLOCATABLE      :: NodeID(:)
  TYPE(tFaceNodes),POINTER :: PrevFaceNodes
  TYPE(tFaceNodes),POINTER :: NextFaceNodes
END TYPE tFaceNodes
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFaceNodesList
  TYPE(tFaceNodes),POINTER :: FirstFaceNodes => NULL()
  TYPE(tFaceNodes),POINTER :: LastFaceNodes  => NULL()
END TYPE tFaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
TYPE tEdge
  TYPE(tNodePtr)         :: Nodes(1:2)
  TYPE(tNodePtr),POINTER :: CurvedNodes(:)
  TYPE(tEdge),POINTER    :: NextEdge
END TYPE tEdge
!----------------------------------------------------------------------------------------------------------------------!
TYPE tEdgePtr
  TYPE(tEdge),POINTER :: Edge
END TYPE tEdgePtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tBC
  INTEGER :: BCType                 
  INTEGER :: BCState
  INTEGER :: BCalphaind
  INTEGER :: BCIndex
END TYPE tBC
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElemList)      :: ElemList
TYPE(tFaceNodesList) :: FaceNodesList
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tMeshInfo)       :: MeshInfo
TYPE(tMeshArraysInfo) :: MeshArraysInfo
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE            :: MeshData_BCIndex(:)
INTEGER,ALLOCATABLE            :: MeshData_BoundaryType(:,:)
CHARACTER(LEN=256),ALLOCATABLE :: MeshData_BoundaryName(:)
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,ALLOCATABLE :: MeshData_ElementsToRefineFlag(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE    :: MeshData_NodesCoordinates(:,:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToFaces(:,:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToLevel(:)
INTEGER,ALLOCATABLE :: MeshData_ElementsToNeighbors(:,:)
INTEGER,ALLOCATABLE :: MeshData_EdgesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_FacesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_BCFacesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_BCFacesToType(:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToNodes(:,:)
INTEGER,ALLOCATABLE :: MeshData_MasterSlavesToElements(:,:)
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE    :: MeshData_ElementsToLevel3D(:,:,:,:,:)
REAL,ALLOCATABLE    :: MeshData_ElementsToNodesCoordinates3D(:,:,:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: QuadMapSideToNodes(1:4,1:2)
INTEGER :: HexaMapSideToNodes(1:6,1:4)
INTEGER :: Quad4QuadMap(1:2,1:6,1:9)
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshMain_vars
!======================================================================================================================!
