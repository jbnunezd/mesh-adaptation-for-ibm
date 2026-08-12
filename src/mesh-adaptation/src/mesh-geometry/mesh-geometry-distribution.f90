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
MODULE MOD_MeshGeometryDistribution
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE DistributeSTLFacetsInElem
  MODULE PROCEDURE DistributeSTLFacetsInElem
END INTERFACE

INTERFACE DistributeSTLFacetsInElem_KDTREE
  MODULE PROCEDURE DistributeSTLFacetsInElem_KDTREE
END INTERFACE

INTERFACE DistributeSTLFacetsInElemList
  MODULE PROCEDURE DistributeSTLFacetsInElemList
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: DistributeSTLFacetsInElem
PUBLIC :: DistributeSTLFacetsInElem_KDTREE
PUBLIC :: DistributeSTLFacetsInElemList
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
SUBROUTINE DistributeSTLFacetsInElemList()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
USE MOD_MeshMainMethods,ONLY: CountNodes
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryFacets
USE MOD_GeometryMain_vars,ONLY: GeometryData_VerticesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nElems
INTEGER            :: nNodes
INTEGER            :: iFacet
INTEGER            :: nFacets
INTEGER            :: iVertex
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
INTEGER            :: MeshInfoData(1:3)
CHARACTER(LEN=255) :: MeshInfoVarNames(1:3)
CHARACTER(LEN=255) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE    :: STLBarycenters(:,:)
INTEGER,ALLOCATABLE :: STLFacetsID(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER              :: KDTree
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=255) :: Header
!----------------------------------------------------------------------------------------------------------------------!

SWRITE(UNIT_SCREEN,*)
Header = "DISTRIBUTING GEOMETRY FACETS IN ELEMLIST..."
CALL PrintMessage(Header)

! Constructing KDTree
CalcTimeIni = RunningTime()

nFacets = SIZE(GeometryData_VerticesCoordinates3D,3)

IF (ALLOCATED(STLBarycenters)) THEN
  DEALLOCATE(STLBarycenters)
END IF
IF (ALLOCATED(STLFacetsID)) THEN
  DEALLOCATE(STLFacetsID)
END IF
ALLOCATE(STLBarycenters(1:3,1:nFacets))
ALLOCATE(STLFacetsID(1:nFacets))

DO iFacet=1,nFacets
  STLBarycenters(1:3,iFacet) = 0.0
  DO iVertex=1,3
    STLBarycenters(1:3,iFacet) = STLBarycenters(1:3,iFacet) + &
                                        GeometryData_VerticesCoordinates3D(iVertex+1,1:3,iFacet)
  END DO
  STLFacetsID(iFacet) = iFacet
  STLBarycenters(1:3,iFacet) = STLBarycenters(1:3,iFacet)/3.0
END DO

CALL ConstructKDTree(KDTree,STLBarycenters,Sort=.FALSE.,Rearrange=.FALSE.)

! Distributing Facets in Mesh
aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  CALL DistributeSTLFacetsInElem_KDTREE(aElem,GeometryFacets,KDTree)
  aElem => aElem%NextElem
END DO

! Compute nElems and nNodes
CALL CountElems(ElemList,nElems)
CALL CountNodes(ElemList,nNodes)

MeshInfoData(1) = nFacets
MeshInfoData(2) = nElems
MeshInfoData(3) = nNodes
MeshInfoVarNames(1) = "nFacets"
MeshInfoVarNames(2) = "nElems"
MeshInfoVarNames(3) = "nNodes"
CALL PrintArrayInfo("Mesh Information",MeshInfoVarNames,MeshInfoData)

CalcTimeEnd = RunningTime()

CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)

Header = "Elapsed Time"
CALL PrintAnalyze(Header,ElapsedTime)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLFacetsInElemList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DistributeSTLFacetsInElem(aElem,Facets)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tFacetCoords
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: BoundingBox
USE MOD_ComputationalGeometry,ONLY: TriangleBoxOverlap
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT)    :: aElem
TYPE(tFacetCoords),TARGET,INTENT(IN) :: Facets(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iFacet
INTEGER :: jFacet
INTEGER :: nContainedFacets
!----------------------------------------------------------------------------------------------------------------------!
REAL :: BoxCenter(1:3)
REAL :: BoxHalfSize(1:3)
REAL :: TriVerts(1:3,1:3)
!----------------------------------------------------------------------------------------------------------------------!
REAL :: alpha
REAL :: dx(1:3)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iPoint
INTEGER :: nPoints
REAL,ALLOCATABLE :: Points(:,:)
REAL,ALLOCATABLE :: ElementBoundingBox(:,:)
!----------------------------------------------------------------------------------------------------------------------!

nPoints = aElem%nNodes

IF (ALLOCATED(Points)) THEN
  DEALLOCATE(Points)
END IF
ALLOCATE(Points(1:nPoints,1:3))

IF (ALLOCATED(ElementBoundingBox)) THEN
  DEALLOCATE(ElementBoundingBox)
END IF
ALLOCATE(ElementBoundingBox(1:2,1:3))

DO iPoint=1,nPoints
  Points(iPoint,1:3) = aElem%Nodes(iPoint)%Node%Coords(1:3)
END DO

ElementBoundingBox = BoundingBox(3,Points)

alpha = ParametersMeshRefinement%ElementEnlargementFactor
dx(1:3) = ABS(ElementBoundingBox(2,1:3)-ElementBoundingBox(1,1:3))
ElementBoundingBox(1,1:3) = ElementBoundingBox(1,1:3) - 0.5*(alpha-1.0)*dx(1:3)
ElementBoundingBox(2,1:3) = ElementBoundingBox(2,1:3) + 0.5*(alpha-1.0)*dx(1:3)

nContainedFacets = 0
DO iFacet=1,SIZE(Facets)
  BoxHalfSize(1:3) = 0.5*(ElementBoundingBox(2,1:3)-ElementBoundingBox(1,1:3))
  BoxCenter(1:3)   = BoxHalfSize(1:3)+ElementBoundingBox(1,1:3)
  TriVerts(1:3,1:3) = Facets(iFacet)%VerticesCoords(1:3,1:3)
  IF (TriangleBoxOverlap(BoxCenter,BoxHalfSize,TriVerts) .EQV. .FALSE.) THEN
    CYCLE
  ELSE
    nContainedFacets = nContainedFacets+1
  END IF
END DO

IF (nContainedFacets .EQ. 0) THEN
  aElem%nFacets = 0
  RETURN
ELSE
  aElem%nFacets = nContainedFacets
  IF (.NOT. ALLOCATED(aElem%Facets)) THEN
    ALLOCATE(aElem%Facets(1:aElem%nFacets))
  END IF
END IF

jFacet = 1
DO iFacet=1,SIZE(Facets)
  BoxHalfSize(1:3) = 0.5*(ElementBoundingBox(2,1:3)-ElementBoundingBox(1,1:3))
  BoxCenter(1:3)   = BoxHalfSize(1:3)+ElementBoundingBox(1,1:3)
  TriVerts(1:3,1:3) = Facets(iFacet)%VerticesCoords(1:3,1:3)
  IF (TriangleBoxOverlap(BoxCenter,BoxHalfSize,TriVerts) .EQV. .FALSE.) THEN
    CYCLE
  ELSE
    aElem%Facets(jFacet)%VerticesCoords(1:3,1:3) = Facets(iFacet)%VerticesCoords(1:3,1:3)
    jFacet = jFacet+1
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLFacetsInElem
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DistributeSTLFacetsInElem_KDTREE(aElem,Facets,KDTree)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tFacetCoords
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: BoundingBox
USE MOD_ComputationalGeometry,ONLY: TriangleBoxOverlap
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER,INTENT(INOUT)    :: aElem
TYPE(tFacetCoords),TARGET,INTENT(IN) :: Facets(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT)  :: KDTree
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors),ALLOCATABLE :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: jFacet
INTEGER :: nContainedFacets
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNeighbor
INTEGER :: nNeighbors
INTEGER :: nNeighborsFound
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Radius
REAL :: Radius2
REAL :: BoxSize
REAL :: BoxCenter(1:3)
REAL :: BoxHalfSize(1:3)
REAL :: TriVerts(1:3,1:3)
REAL :: QueryPoint(1:3)
!----------------------------------------------------------------------------------------------------------------------!
REAL :: alpha
REAL :: dx(1:3)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iPoint
INTEGER :: nPoints
REAL,ALLOCATABLE :: Points(:,:)
REAL,ALLOCATABLE :: ElementBoundingBox(:,:)
!----------------------------------------------------------------------------------------------------------------------!

nPoints = aElem%nNodes

IF (ALLOCATED(Points)) THEN
  DEALLOCATE(Points)
END IF
ALLOCATE(Points(1:nPoints,1:3))

IF (ALLOCATED(ElementBoundingBox)) THEN
  DEALLOCATE(ElementBoundingBox)
END IF
ALLOCATE(ElementBoundingBox(1:2,1:3))

DO iPoint=1,nPoints
  Points(iPoint,1:3) = aElem%Nodes(iPoint)%Node%Coords(1:3)
END DO

ElementBoundingBox = BoundingBox(3,Points)

alpha = ParametersMeshRefinement%ElementEnlargementFactor
dx(1:3) = ABS(ElementBoundingBox(2,1:3)-ElementBoundingBox(1,1:3))
ElementBoundingBox(1,1:3) = ElementBoundingBox(1,1:3) - 0.5*(alpha-1.0)*dx(1:3)
ElementBoundingBox(2,1:3) = ElementBoundingBox(2,1:3) + 0.5*(alpha-1.0)*dx(1:3)

BoxHalfSize(1:3) = 0.5*(ElementBoundingBox(2,1:3)-ElementBoundingBox(1,1:3))
BoxCenter(1:3)   = BoxHalfSize(1:3)+ElementBoundingBox(1,1:3)
QueryPoint(1:3)  = BoxCenter(1:3)

BoxSize = ParametersMeshRefinement%TessellationMaxEdgeLength
Radius  = 10.0*BoxSize
Radius2 = Radius**2
nNeighbors = CountNearestNeighborsInsideBallAroundQueryPoint(KDTree,QueryPoint,Radius2)
IF (ALLOCATED(SearchResults)) THEN
  DEALLOCATE(SearchResults)
END IF
ALLOCATE(SearchResults(1:nNeighbors))
CALL FindNearestNeighborsInsideBallAroundQueryPoint(KDTree,QueryPoint,Radius2,nNeighbors,nNeighborsFound,SearchResults)
nContainedFacets = 0
DO iNeighbor=1,nNeighbors
  TriVerts(1:3,1:3) = Facets(SearchResults(iNeighbor)%NeighborIndex)%VerticesCoords(1:3,1:3)
  IF (TriangleBoxOverlap(BoxCenter,BoxHalfSize,TriVerts) .EQV. .FALSE.) THEN
    CYCLE
  ELSE
    nContainedFacets = nContainedFacets+1
  END IF
END DO

IF (nContainedFacets .EQ. 0) THEN
  aElem%nFacets = 0
  RETURN
ELSE
  aElem%nFacets = nContainedFacets
  IF (.NOT. ALLOCATED(aElem%Facets)) THEN
    ALLOCATE(aElem%Facets(1:aElem%nFacets))
  END IF
END IF

BoxHalfSize(1:3) = 0.5*(ElementBoundingBox(2,1:3)-ElementBoundingBox(1,1:3))
BoxCenter(1:3)   = BoxHalfSize(1:3)+ElementBoundingBox(1,1:3)
QueryPoint(1:3)  = BoxCenter(1:3)

BoxSize = ParametersMeshRefinement%TessellationMaxEdgeLength
Radius  = 10.0*BoxSize
Radius2 = Radius**2
nNeighbors = CountNearestNeighborsInsideBallAroundQueryPoint(KDTree,QueryPoint,Radius2)
IF (ALLOCATED(SearchResults)) THEN
  DEALLOCATE(SearchResults)
END IF
ALLOCATE(SearchResults(1:nNeighbors))
CALL FindNearestNeighborsInsideBallAroundQueryPoint(KDTree,QueryPoint,Radius2,nNeighbors,nNeighborsFound,SearchResults)

jFacet = 1
DO iNeighbor=1,nNeighbors
  TriVerts(1:3,1:3) = Facets(SearchResults(iNeighbor)%NeighborIndex)%VerticesCoords(1:3,1:3)
  IF (TriangleBoxOverlap(BoxCenter,BoxHalfSize,TriVerts) .EQV. .FALSE.) THEN
    CYCLE
  ELSE
    aElem%Facets(jFacet)%VerticesCoords(1:3,1:3) = TriVerts(1:3,1:3)
    jFacet = jFacet+1
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DistributeSTLFacetsInElem_KDTREE
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshGeometryDistribution
!======================================================================================================================!
