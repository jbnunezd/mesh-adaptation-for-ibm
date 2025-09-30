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
MODULE MOD_MeshRefinement_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshRefinement
  REAL               :: TessellationMaxEdgeLength
  REAL               :: RegionEnlargementFactor
  REAL               :: ElementEnlargementFactor
  REAL               :: RefinedBoxCorner3D(1:8,1:3) = 0.0
  INTEGER            :: MaxRefinementLevel
  LOGICAL            :: DebugMeshRefinement = .FALSE.
  LOGICAL            :: IsotropicRefinement = .FALSE.
  CHARACTER(LEN=256) :: WhichMeshRefinement
  CHARACTER(LEN=256) :: WhichRefinementPlane
  CHARACTER(LEN=256) :: WhichBoxedRegion
  INTEGER                        :: nRefinedBox
  INTEGER,ALLOCATABLE            :: MaxBoxRefinementLevel(:)
  REAL,ALLOCATABLE               :: RefinedBoxCorner(:,:,:)
  CHARACTER(LEN=256),ALLOCATABLE :: RefinedBoxName(:)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshRefinement) :: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
PROCEDURE(),POINTER :: FlagElementsForRefinement
PROCEDURE(),POINTER :: FlagElementsInsideBox
PROCEDURE(),POINTER :: FlagElementsAroundGeometry
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: MaxRefinementLevel
INTEGER,ALLOCATABLE :: MaxBoxRefinementLevel(:)
LOGICAL             :: RefineMeshInsideBox      = .FALSE.
LOGICAL             :: RefineMeshAroundGeometry = .FALSE.
LOGICAL             :: IsotropicRefinement      = .FALSE.
CHARACTER(LEN=256)  :: WhichMeshRefinement
CHARACTER(LEN=256)  :: WhichRefinementPlane
CHARACTER(LEN=256)  :: WhichBoxedRegion
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: InitializeMeshRefinementIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinement_vars
!======================================================================================================================!
