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
MODULE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE BuildPhysicalDomain3D_INT(Nin,nElems,nBoxElems,BoxCorners,XRefDom3D,XPhysDom3D)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: Nin
    INTEGER,INTENT(IN) :: nElems
    INTEGER,INTENT(IN) :: nBoxElems(1:3)
    REAL,INTENT(IN)    :: BoxCorners(1:8,1:3)
    REAL,INTENT(IN)    :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
    REAL,INTENT(OUT)   :: XPhysDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
  END SUBROUTINE BuildPhysicalDomain3D_INT
END INTERFACE
PROCEDURE(BuildPhysicalDomain3D_INT),POINTER :: BuildPhysicalDomain3D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE Mapping3D_INT(WhichFace,s,xc,x)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: WhichFace
    REAL,INTENT(IN)    :: s(1:2)
    REAL,INTENT(IN)    :: xc(1:8,1:3)
    REAL,INTENT(OUT)   :: x(1:3)
  END SUBROUTINE Mapping3D_INT
END INTERFACE
PROCEDURE(Mapping3D_INT),POINTER :: Mapping3D
!----------------------------------------------------------------------------------------------------------------------!
ABSTRACT INTERFACE
  SUBROUTINE MeshStretching3D_INT(nElems,dx,dy,dz)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: nElems(1:3)
    REAL,INTENT(OUT)   :: dx(1:nElems(1))
    REAL,INTENT(OUT)   :: dy(1:nElems(2))
    REAL,INTENT(OUT)   :: dz(1:nElems(3))
  END SUBROUTINE MeshStretching3D_INT
END INTERFACE
PROCEDURE(MeshStretching3D_INT),POINTER :: MeshStretching3D
!----------------------------------------------------------------------------------------------------------------------!
INTEGER           :: nBoxElems3D(1:3)
REAL              :: BoxCorner3D(1:8,1:3)
REAL,ALLOCATABLE  :: CGL_NGeo_Domain3D(:,:,:,:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE tParametersMeshBuiltIn3D
  INTEGER :: nBoxElems3D(1:3)
  REAL    :: BoxCorner3D(1:8,1:3)
  LOGICAL :: StretchMesh
  LOGICAL :: DebugMeshBuiltIn = .FALSE.
  INTEGER :: BCIndex(1:6)
  INTEGER,ALLOCATABLE :: BoundaryType(:,:)
  CHARACTER(LEN=256),ALLOCATABLE :: BoundaryName(:)
  CHARACTER(LEN=256)  :: WhichMapping3D
  CHARACTER(LEN=256)  :: WhichMeshType
  CHARACTER(LEN=256)  :: WhichOutputBasis
  CHARACTER(LEN=256)  :: WhichMeshStretching
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tParametersMeshBuiltIn3D) :: ParametersMeshBuiltIn3D
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
! Chebyshev-Gauss-Lobatto (NGeo)
REAL,ALLOCATABLE :: CGL_xNodes_NGeo(:)
REAL,ALLOCATABLE :: CGL_xWeights_NGeo(:)
REAL,ALLOCATABLE :: CGL_xBaryWeights_NGeo(:)
REAL,ALLOCATABLE :: CGL_DMatrix_NGeo(:,:)

! Uniform (N)
REAL,ALLOCATABLE :: UNIFORM_xNodes_N(:)
REAL,ALLOCATABLE :: VDM_CGLNGeo_UniformN(:,:)
! Chebyshev-Gauss-Lobatto (NGeo) to OutputBasis (N)
REAL,ALLOCATABLE :: VDM_CGLNGeo_OutputN(:,:)
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: InitializeMeshBuiltInIsDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshBuiltIn_vars
!======================================================================================================================!
