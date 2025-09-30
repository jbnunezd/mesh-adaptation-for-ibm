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
MODULE MOD_MeshRefinementFlagging
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeElementsToRefineArray
  MODULE PROCEDURE InitializeElementsToRefineArray
END INTERFACE

INTERFACE FlagElementsForBalancing
  MODULE PROCEDURE FlagElementsForBalancing
END INTERFACE

INTERFACE FlagAllElements
  MODULE PROCEDURE FlagAllElements
END INTERFACE

INTERFACE FlagElementsInsideStandardBox
  MODULE PROCEDURE FlagElementsInsideStandardBox
END INTERFACE

INTERFACE FlagElementsInsideAdaptiveBox
  MODULE PROCEDURE FlagElementsInsideAdaptiveBox
END INTERFACE

INTERFACE FlagElementsOverlapFacets
  MODULE PROCEDURE FlagElementsOverlapFacets
END INTERFACE

INTERFACE FlagElementsAroundGeometryAndInsideBox
  MODULE PROCEDURE FlagElementsAroundGeometryAndInsideBox
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeElementsToRefineArray
PUBLIC :: FlagElementsForBalancing
PUBLIC :: FlagAllElements
PUBLIC :: FlagElementsInsideStandardBox
PUBLIC :: FlagElementsInsideAdaptiveBox
PUBLIC :: FlagElementsOverlapFacets
PUBLIC :: FlagElementsAroundGeometryAndInsideBox
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
SUBROUTINE InitializeElementsToRefineArray()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMainMethods,ONLY: CountElems
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nElems
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

! Compute nElems
CALL CountElems(ElemList,nElems)

IF (ALLOCATED(MeshData_ElementsToRefineFlag)) THEN
  DEALLOCATE(MeshData_ElementsToRefineFlag)
END IF
ALLOCATE(MeshData_ElementsToRefineFlag(1:nElems))

MeshData_ElementsToRefineFlag(1:nElems) = .FALSE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeElementsToRefineArray
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsForBalancing(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: IsotropicRefinement
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (IsotropicRefinement .EQV. .FALSE.) THEN
  CALL FlagElementsForBalancing3D_XY(Level)
ELSE
  CALL FlagElementsForBalancing3D(Level)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsForBalancing
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsForBalancing3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLevel
INTEGER :: iElem(1:5)
INTEGER :: iQuad4Quad
INTEGER :: nQuad4Quad
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(MeshData_MasterSlavesToElements)) THEN
  nQuad4Quad = 0
ELSE 
  nQuad4Quad = SIZE(MeshData_MasterSlavesToElements,1)
END IF

IF (nQuad4Quad .EQ. 0) THEN
  RETURN
END IF

IF (Level .GT. 1) THEN
  DO iLevel=Level-1,1,-1
    DO iQuad4Quad=1,nQuad4Quad
      iElem(1:5) = MeshData_MasterSlavesToElements(iQuad4Quad,1:5)
      IF (((MeshData_ElementsToRefineFlag(iElem(2)) .EQV. .TRUE.) .OR. &
           (MeshData_ElementsToRefineFlag(iElem(3)) .EQV. .TRUE.) .OR. &
           (MeshData_ElementsToRefineFlag(iElem(4)) .EQV. .TRUE.) .OR. &
           (MeshData_ElementsToRefineFlag(iElem(5)) .EQV. .TRUE.)) .AND. &
           (MeshData_ElementsToLevel(iElem(1)) .EQ. iLevel-1)) THEN
        MeshData_ElementsToRefineFlag(iElem(1)) = .TRUE.
      END IF
    END DO
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsForBalancing3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsForBalancing3D_XY(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToLevel
USE MOD_MeshMain_vars,ONLY: MeshData_MasterSlavesToElements
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLevel
INTEGER :: iElem(1:3)
INTEGER :: iQuad2Quad
INTEGER :: nQuad2Quad
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ALLOCATED(MeshData_MasterSlavesToElements)) THEN
  nQuad2Quad = 0
ELSE 
  nQuad2Quad = SIZE(MeshData_MasterSlavesToElements,1)
END IF

IF (nQuad2Quad .EQ. 0) THEN
  RETURN
END IF

IF (Level .GT. 1) THEN
  DO iLevel=Level-1,1,-1
    DO iQuad2Quad=1,nQuad2Quad
      iElem(1:3) = MeshData_MasterSlavesToElements(iQuad2Quad,1:3)
      IF (((MeshData_ElementsToRefineFlag(iElem(2)) .EQV. .TRUE.) .OR. &
           (MeshData_ElementsToRefineFlag(iElem(3)) .EQV. .TRUE.)) .AND. &
           (MeshData_ElementsToLevel(iElem(1)) .EQ. iLevel-1)) THEN
        MeshData_ElementsToRefineFlag(iElem(1)) = .TRUE.
      END IF
    END DO
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsForBalancing3D_XY
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagAllElements(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL FlagAllElements3D(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagAllElements
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagAllElements3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
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

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
  aElem => aElem%NextElem
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagAllElements3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideStandardBox(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL FlagElementsInsideStandardBox3D(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideStandardBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideStandardBox3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,  ONLY: ParametersMeshRefinement
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: ElemID
REAL    :: RefinedBox3D(1:8,1:3)
REAL    :: BaryCenterCoords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!

nRefinedBox = ParametersMeshRefinement%nRefinedBox
DO iRefinedBox=1,nRefinedBox
  IF (Level .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)) THEN
    CYCLE
  END IF
  RefinedBox3D(1:8,1:3) = ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,1:8,1:3)
  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    ElemID = aElem%ElemID
    BaryCenterCoords(1:3) = 0.0
    DO iNode=1,aElem%nNodes
      BaryCenterCoords(1:3) = BaryCenterCoords(1:3) + aElem%Nodes(iNode)%Node%Coords(1:3)
    END DO
    BaryCenterCoords(1:3) = 0.125*BaryCenterCoords(1:3)
    IF (PointIsInsideCube3D(BaryCenterCoords,RefinedBox3D) .EQV. .TRUE.) THEN
      MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
    END IF
    aElem => aElem%NextElem
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideStandardBox3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideAdaptiveBox(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL FlagElementsInsideAdaptiveBox3D(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideAdaptiveBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsInsideAdaptiveBox3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,  ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ComputationalGeometry,ONLY: PointIsInsideCube3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
! ! ! INTEGER :: iLevel
INTEGER :: ElemID
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: AdaptiveBox3D(1:8,1:3)
REAL    :: RefinedBox3D(1:8,1:3)
! ! ! REAL    :: BaryCenterCoords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tElem),POINTER :: aElem
!----------------------------------------------------------------------------------------------------------------------!
REAL :: Delta
REAL :: AlphaX
REAL :: AlphaY
REAL :: AlphaZ
REAL :: dx
REAL :: dy
REAL :: dz
!----------------------------------------------------------------------------------------------------------------------!
nRefinedBox = ParametersMeshRefinement%nRefinedBox
DO iRefinedBox=1,nRefinedBox
  IF (Level .GT. ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)) THEN
    CYCLE
  END IF

  RefinedBox3D(1:8,1:3) = ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,1:8,1:3)

  !**********************************************************!
  Delta  = ParametersMeshRefinement%RegionEnlargementFactor
  AlphaX = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)
  AlphaY = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)
  AlphaZ = 2**(ParametersMeshRefinement%MaxBoxRefinementLevel(iRefinedBox)-Level+1)
  !**********************************************************!
  dx = Delta
  dy = Delta
  dz = Delta
  !**********************************************************!
  ! Face 5 (X-)
  AdaptiveBox3D(1,1) = RefinedBox3D(1,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(4,1) = RefinedBox3D(4,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(5,1) = RefinedBox3D(5,1) - 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(8,1) = RefinedBox3D(8,1) - 0.5*(AlphaX-1.0)*dx
  ! Face 3 (X+)
  AdaptiveBox3D(2,1) = RefinedBox3D(2,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(3,1) = RefinedBox3D(3,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(6,1) = RefinedBox3D(6,1) + 0.5*(AlphaX-1.0)*dx
  AdaptiveBox3D(7,1) = RefinedBox3D(7,1) + 0.5*(AlphaX-1.0)*dx
  ! Face 2 (Y-)
  AdaptiveBox3D(1,2) = RefinedBox3D(1,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(2,2) = RefinedBox3D(2,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(5,2) = RefinedBox3D(5,2) - 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(6,2) = RefinedBox3D(6,2) - 0.5*(AlphaY-1.0)*dy
  ! Face 4 (Y+)
  AdaptiveBox3D(3,2) = RefinedBox3D(3,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(4,2) = RefinedBox3D(4,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(7,2) = RefinedBox3D(7,2) + 0.5*(AlphaY-1.0)*dy
  AdaptiveBox3D(8,2) = RefinedBox3D(8,2) + 0.5*(AlphaY-1.0)*dy
  ! Face 1 (Z-)
  AdaptiveBox3D(1,3) = RefinedBox3D(1,3) - 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(2,3) = RefinedBox3D(2,3) - 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(3,3) = RefinedBox3D(3,3) - 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(4,3) = RefinedBox3D(4,3) - 0.5*(AlphaZ-1.0)*dz
  ! Face 6 (Z+)
  AdaptiveBox3D(5,3) = RefinedBox3D(5,3) + 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(6,3) = RefinedBox3D(6,3) + 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(7,3) = RefinedBox3D(7,3) + 0.5*(AlphaZ-1.0)*dz
  AdaptiveBox3D(8,3) = RefinedBox3D(8,3) + 0.5*(AlphaZ-1.0)*dz
  !**********************************************************!

  aElem => ElemList%FirstElem
  DO WHILE (ASSOCIATED(aElem))
    ElemID = aElem%ElemID
    DO iNode=1,aElem%nNodes
      IF (PointIsInsideCube3D(aElem%Nodes(iNode)%Node%Coords(1:3),AdaptiveBox3D) .EQV. .TRUE.) THEN
        MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
        EXIT
      END IF
    END DO
    aElem => aElem%NextElem
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsInsideAdaptiveBox3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsAroundGeometryAndInsideBox(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: FlagElementsInsideBox
USE MOD_MeshRefinement_vars,ONLY: FlagElementsAroundGeometry
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL FlagElementsInsideBox(Level)
CALL FlagElementsAroundGeometry(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsAroundGeometryAndInsideBox
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsOverlapFacets(Level)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Level
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL FlagElementsOverlapFacets3D(Level)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsOverlapFacets
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FlagElementsOverlapFacets3D(Level)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: tElem
USE MOD_MeshMain_vars,ONLY: ElemList
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToRefineFlag
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

aElem => ElemList%FirstElem
DO WHILE (ASSOCIATED(aElem))
  ElemID = aElem%ElemID
  IF (aElem%nFacets .EQ. 0) THEN
    aElem => aElem%NextElem
    CYCLE
  END IF
  IF (aElem%nFacets .GE. 1) THEN
    MeshData_ElementsToRefineFlag(ElemID) = .TRUE.
    aElem => aElem%NextElem
    CYCLE
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FlagElementsOverlapFacets3D
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinementFlagging
!======================================================================================================================!
