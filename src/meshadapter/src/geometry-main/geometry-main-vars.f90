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
MODULE MOD_GeometryMain_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE tPointCoords
  INTEGER :: PointID
  REAL    :: Coords(1:3)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFacetCoords
  REAL :: VerticesCoords(1:3,1:3)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFacetNode
  REAL    :: Coords(1:3)
  INTEGER :: NodeID
  INTEGER :: RefCount
  INTEGER :: tmp
END TYPE tFacetNode
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFacetNodePtr
  TYPE(tFacetNode),POINTER :: Node
END TYPE tFacetNodePtr
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFacet
  INTEGER :: FacetID
  TYPE(tFacet),POINTER        :: PrevFacet
  TYPE(tFacet),POINTER        :: NextFacet
  TYPE(tFacetNodePtr),POINTER :: Nodes(:)
END TYPE tFacet
!----------------------------------------------------------------------------------------------------------------------!
TYPE tFacetList
  TYPE(tFacet),POINTER :: FirstFacet => NULL()
  TYPE(tFacet),POINTER :: LastFacet  => NULL()
END TYPE tFacetList
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFacetList) :: FacetList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,ALLOCATABLE :: GeometryData_FacetsToNodes(:,:)
REAL,ALLOCATABLE    :: GeometryData_NodesCoordinates(:,:)
REAL,ALLOCATABLE    :: GeometryData_PointsCoordinates3D(:,:)
REAL,ALLOCATABLE    :: GeometryData_VerticesCoordinates3D(:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPointCoords),ALLOCATABLE :: GeometryPoints(:)
TYPE(tFacetCoords),ALLOCATABLE :: GeometryFacets(:)
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_GeometryMain_vars
!======================================================================================================================!
