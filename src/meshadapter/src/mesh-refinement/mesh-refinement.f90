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
MODULE MOD_MeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshRefinement
  MODULE PROCEDURE InitializeMeshRefinement
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshRefinement
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
SUBROUTINE InitializeMeshRefinement()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_nDims
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: MaxRefinementLevel
USE MOD_MeshRefinement_vars,ONLY: MaxBoxRefinementLevel
USE MOD_MeshRefinement_vars,ONLY: WhichBoxedRegion
USE MOD_MeshRefinement_vars,ONLY: WhichMeshRefinement
USE MOD_MeshRefinement_vars,ONLY: WhichRefinementPlane
USE MOD_MeshRefinement_vars,ONLY: IsotropicRefinement
USE MOD_MeshRefinement_vars,ONLY: RefineMeshInsideBox
USE MOD_MeshRefinement_vars,ONLY: RefineMeshAroundGeometry
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
USE MOD_MeshRefinement_vars,ONLY: FlagElementsForRefinement
USE MOD_MeshRefinement_vars,ONLY: FlagElementsInsideBox
USE MOD_MeshRefinement_vars,ONLY: FlagElementsAroundGeometry
USE MOD_MeshRefinement_vars,ONLY: InitializeMeshRefinementIsDone
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinementFlagging,ONLY: FlagAllElements
USE MOD_MeshRefinementFlagging,ONLY: FlagElementsInsideStandardBox
USE MOD_MeshRefinementFlagging,ONLY: FlagElementsInsideAdaptiveBox
USE MOD_MeshRefinementFlagging,ONLY: FlagElementsOverlapFacets
USE MOD_MeshRefinementFlagging,ONLY: FlagElementsAroundGeometryAndInsideBox
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometry_vars,ONLY: WhichBodyGeometry
USE MOD_MeshGeometry_vars,ONLY: WhichGeometryDistribution
USE MOD_MeshGeometry_vars,ONLY: DistributeSTLDataInElemList
USE MOD_MeshGeometry_vars,ONLY: ParametersMeshGeometry
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshGeometryDistribution,ONLY: DistributeSTLFacetsInElemList
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshRefinement_vars,ONLY: ParametersMeshRefinement
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
INTEGER :: iRefinedBox
INTEGER :: nRefinedBox
INTEGER :: nRefinedBoxCorners
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshRefinementIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshRefinement not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING MESHINFO MODULE..."
CALL PrintHeader(Header)

ParametersMeshRefinement%WhichMeshRefinement       = GetString('WhichMeshRefinement')
ParametersMeshRefinement%DebugMeshRefinement       = GetLogical('DebugMeshRefinement','.FALSE.')
ParametersMeshRefinement%IsotropicRefinement       = GetLogical('IsotropicRefinement','.TRUE.')
ParametersMeshRefinement%MaxRefinementLevel        = GetInteger('MaxRefinementLevel','0')
ParametersMeshRefinement%nRefinedBox               = GetInteger('nRefinedBox','0')
ParametersMeshRefinement%TessellationMaxEdgeLength = GetReal('TessellationMaxEdgeLength')
ParametersMeshRefinement%ElementEnlargementFactor  = GetReal('ElementEnlargementFactor','1.0')
ParametersMeshRefinement%RegionEnlargementFactor   = GetReal('RegionEnlargementFactor','1.0')

MaxRefinementLevel  = ParametersMeshRefinement%MaxRefinementLevel
WhichMeshRefinement = ParametersMeshRefinement%WhichMeshRefinement

IsotropicRefinement = ParametersMeshRefinement%IsotropicRefinement
IF (IsotropicRefinement .EQV. .FALSE.) THEN
  ParametersMeshRefinement%WhichRefinementPlane = GetString('WhichRefinementPlane')
  WhichRefinementPlane = ParametersMeshRefinement%WhichRefinementPlane
END IF

SELECT CASE(LowerCase(WhichMeshRefinement))
  CASE("refine-all-elements")
    FlagElementsForRefinement => FlagAllElements

  CASE("refine-elements-inside-box")
    RefineMeshInsideBox = .TRUE.
    ParametersMeshRefinement%WhichBoxedRegion = GetString('WhichBoxedRegion')
    WhichBoxedRegion = ParametersMeshRefinement%WhichBoxedRegion
    SELECT CASE(LowerCase(WhichBoxedRegion))
      CASE("standard-box")
        FlagElementsForRefinement => FlagElementsInsideStandardBox
      CASE("adaptive-box")
        FlagElementsForRefinement => FlagElementsInsideAdaptiveBox
      CASE DEFAULT
      ErrorMessage = "InitializeMeshRefinement: Unknown STL Data Distribution"
      CALL PrintError(__STAMP__,ErrorMessage)
    END SELECT

  CASE("refine-elements-around-geometry")
    RefineMeshAroundGeometry = .TRUE.
    ParametersMeshGeometry%WhichBodyGeometry = GetString('WhichBodyGeometry')
    WhichBodyGeometry = ParametersMeshGeometry%WhichBodyGeometry
    SELECT CASE(LowerCase(WhichBodyGeometry))
      CASE("imported-geometry")
        ParametersMeshGeometry%WhichGeometryDistribution = GetString('WhichGeometryDistribution')
        WhichGeometryDistribution = ParametersMeshGeometry%WhichGeometryDistribution
        SELECT CASE(LowerCase(WhichGeometryDistribution))
          CASE("stl-facets-distribution")
            FlagElementsForRefinement   => FlagElementsOverlapFacets
            DistributeSTLDataInElemList => DistributeSTLFacetsInElemList
          CASE DEFAULT
          ErrorMessage = "InitializeMeshRefinement: Unknown STL Data Distribution"
          CALL PrintError(__STAMP__,ErrorMessage)
        END SELECT
      CASE DEFAULT
      ErrorMessage = "InitializeMeshRefinement: Unknown Body Geometry"
      CALL PrintError(__STAMP__,ErrorMessage)
    END SELECT

  CASE("refine-elements-around-geometry-and-inside-box")
    RefineMeshAroundGeometry  = .TRUE.
    RefineMeshInsideBox       = .TRUE.
    FlagElementsForRefinement => FlagElementsAroundGeometryAndInsideBox

    ParametersMeshGeometry%WhichBodyGeometry = GetString('WhichBodyGeometry')
    WhichBodyGeometry   = ParametersMeshGeometry%WhichBodyGeometry

    SELECT CASE(LowerCase(WhichBodyGeometry))
      CASE("imported-geometry")
        ParametersMeshGeometry%WhichGeometryDistribution = GetString('WhichGeometryDistribution')
        WhichGeometryDistribution = ParametersMeshGeometry%WhichGeometryDistribution
        SELECT CASE(LowerCase(WhichGeometryDistribution))
          CASE("stl-facets-distribution")
            FlagElementsAroundGeometry  => FlagElementsOverlapFacets
            DistributeSTLDataInElemList => DistributeSTLFacetsInElemList
          CASE DEFAULT
          ErrorMessage = "InitializeMeshRefinement: Unknown STL Data Distribution"
          CALL PrintError(__STAMP__,ErrorMessage)
        END SELECT
      CASE DEFAULT
      ErrorMessage = "InitializeMeshRefinement: Unknown Body Geometry"
      CALL PrintError(__STAMP__,ErrorMessage)
    END SELECT
    
    ParametersMeshRefinement%WhichBoxedRegion = GetString('WhichBoxedRegion')
    WhichBoxedRegion = ParametersMeshRefinement%WhichBoxedRegion
    SELECT CASE(LowerCase(WhichBoxedRegion))
      CASE("standard-box")
        FlagElementsInsideBox => FlagElementsInsideStandardBox
      CASE("adaptive-box")
        FlagElementsInsideBox => FlagElementsInsideAdaptiveBox
      CASE DEFAULT
      ErrorMessage = "InitializeMeshRefinement: Unknown STL Data Distribution"
      CALL PrintError(__STAMP__,ErrorMessage)
    END SELECT

  CASE DEFAULT
  ErrorMessage = "InitializeMeshRefinement: Unknown Mesh Refinement task"
  CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

IF (RefineMeshInsideBox .EQV. .TRUE.) THEN
  nRefinedBox = ParametersMeshRefinement%nRefinedBox
  IF (nRefinedBox .GT. 0) THEN
    nRefinedBoxCorners = CountStrings('RefinedBoxCorner',0)
    IF (nRefinedBoxCorners .LT. nRefinedBox*(2**PP_nDims)) THEN
      ErrorMessage = "InitializeMeshRefinement: Wrong number of RefinedBoxCorner"
      CALL PrintError(__STAMP__,ErrorMessage)
    END IF
    IF (ALLOCATED(ParametersMeshRefinement%RefinedBoxName) .EQV. .FALSE.) THEN
      ALLOCATE(ParametersMeshRefinement%RefinedBoxName(1:nRefinedBox))
    END IF
    IF (ALLOCATED(ParametersMeshRefinement%RefinedBoxCorner) .EQV. .FALSE.) THEN
      ALLOCATE(ParametersMeshRefinement%RefinedBoxCorner(1:nRefinedBox,1:2**PP_nDims,1:PP_nDims))
    END IF
    
    IF (ALLOCATED(ParametersMeshRefinement%MaxBoxRefinementLevel) .EQV. .FALSE.) THEN
      ALLOCATE(MaxBoxRefinementLevel(1:nRefinedBox))
      ALLOCATE(ParametersMeshRefinement%MaxBoxRefinementLevel(1:nRefinedBox))
    END IF
    ParametersMeshRefinement%MaxBoxRefinementLevel = GetIntegerArray('MaxBoxRefinementLevel',nRefinedBox)
    MaxBoxRefinementLevel = ParametersMeshRefinement%MaxBoxRefinementLevel
    ! WARNING: MaxBoxRefinementLevel
    IF (ParametersMeshRefinement%MaxRefinementLevel .LT. MAXVAL(MaxBoxRefinementLevel)) THEN
      ParametersMeshRefinement%MaxRefinementLevel = MAXVAL(MaxBoxRefinementLevel)
      MaxRefinementLevel = ParametersMeshRefinement%MaxRefinementLevel
    END IF
    ! WARNING: MaxBoxRefinementLevel
    DO iRefinedBox=1,nRefinedBox
      ParametersMeshRefinement%RefinedBoxName(iRefinedBox) = GetString('RefinedBoxName')
      DO i=1,2**PP_nDims
        ParametersMeshRefinement%RefinedBoxCorner(iRefinedBox,i,:) = GetRealArray('RefinedBoxCorner',PP_nDims)
      END DO
    END DO
  END IF
END IF

InitializeMeshRefinementIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshRefinement
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshRefinement
!======================================================================================================================!
