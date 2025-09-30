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
MODULE MOD_MeshAdapter_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshAdapter
  INTEGER            :: MeshDimension
  CHARACTER(LEN=256) :: FileVersion
  CHARACTER(LEN=256) :: ProgramName
  CHARACTER(LEN=256) :: ProjectName
  CHARACTER(LEN=256) :: WhichMeshConstructionMethod
  CHARACTER(LEN=256),ALLOCATABLE :: MeshOutputFormat(:)
  LOGICAL            :: VisualizeMesh = .FALSE.
  LOGICAL            :: VisualizeMeshAndData = .FALSE.
  LOGICAL            :: RefineMeshAroundGeometry = .FALSE.
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshAdapter) :: ParametersMeshAdapter
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: InitializeMeshAdapterIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshAdapter_vars
!======================================================================================================================!
