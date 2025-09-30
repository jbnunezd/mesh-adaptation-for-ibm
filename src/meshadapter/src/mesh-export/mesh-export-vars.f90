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
MODULE MOD_MeshExport_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshExport
  LOGICAL            :: ExportMesh = .FALSE.
  LOGICAL            :: ExportMeshAndData = .FALSE.
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshExport) :: ParametersMeshExport
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256),ALLOCATABLE :: MeshExportNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: MeshExportFormat(:)
INTEGER,           ALLOCATABLE :: MeshExportFormatIndex(:)
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: InitializeMeshExportIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshExport_vars
!======================================================================================================================!
