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
MODULE MOD_GeometryImport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeGeometryImport
  MODULE PROCEDURE InitializeGeometryImport
END INTERFACE

INTERFACE GeometryImport
  MODULE PROCEDURE GeometryImport
END INTERFACE

INTERFACE ExtractPointsFromGeometryImport
  MODULE PROCEDURE ExtractPointsFromGeometryImport
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeGeometryImport
PUBLIC :: GeometryImport
PUBLIC :: ExtractPointsFromGeometryImport
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
SUBROUTINE InitializeGeometryImport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryImport_vars,ONLY: ParametersGeometryImport
USE MOD_GeometryImport_vars,ONLY: InitializeGeometryImportIsDone
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeGeometryImportIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeGeometry not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING GEOMETRY IMPORT MODULE..."
CALL PrintHeader(Header)

ParametersGeometryImport%InputGeometryFile       = GetString('InputGeometryFile')
ParametersGeometryImport%InputGeometryFileFormat = GetString('InputGeometryFileFormat')
ParametersGeometryImport%DebugGeometryImport     = GetLogical('DebugGeometryImport','.FALSE.')

InitializeGeometryImportIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeGeometryImport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GeometryImport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryImport_STL
USE MOD_GeometryImport_vars,ONLY: ParametersGeometryImport
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: GeometryData_VerticesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: Debug
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: InputFile
CHARACTER(LEN=256) :: FileFormat
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

Debug      = ParametersGeometryImport%DebugGeometryImport
InputFile  = TRIM(ParametersGeometryImport%InputGeometryFile)
FileFormat = TRIM(ParametersGeometryImport%InputGeometryFileFormat)

! Read in geometry from file
SELECT CASE(LowerCase(FileFormat))
  CASE("stl")
    CALL GeometryImport_STL(InputFile,GeometryData_VerticesCoordinates3D,Debug)
  CASE DEFAULT
  ErrorMessage = "Unknown geometry format"
  CALL PrintError(__STAMP__,ErrorMessage)  
END SELECT

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryImport
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ExtractPointsFromGeometryImport()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMain_vars,ONLY: tFacet
USE MOD_GeometryMain_vars,ONLY: tFacetNodePtr
USE MOD_GeometryMain_vars,ONLY: FacetList
USE MOD_GeometryMain_vars,ONLY: GeometryPoints
USE MOD_GeometryMain_vars,ONLY: GeometryFacets
USE MOD_GeometryMain_vars,ONLY: GeometryData_FacetsToNodes
USE MOD_GeometryMain_vars,ONLY: GeometryData_NodesCoordinates
USE MOD_GeometryMain_vars,ONLY: GeometryData_VerticesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GeometryMainMethods,ONLY: CreateFacet
USE MOD_GeometryMainMethods,ONLY: RemoveFacet
USE MOD_GeometryMainMethods,ONLY: CountFacets
USE MOD_GeometryMainMethods,ONLY: CountFacetNodes
USE MOD_GeometryMainMethods,ONLY: CreateFacetNode
USE MOD_GeometryMainMethods,ONLY: AddDataToFacetNode
USE MOD_GeometryMainMethods,ONLY: GetFacetNodeData
USE MOD_GeometryMainMethods,ONLY: RemoveFacetNode
USE MOD_GeometryMainMethods,ONLY: AddFacetToList
USE MOD_GeometryMainMethods,ONLY: PrintFacetList
USE MOD_GeometryMainMethods,ONLY: DestructFacetList
USE MOD_GeometryMainMethods,ONLY: SetUniqueFacetID
USE MOD_GeometryMainMethods,ONLY: SetUniqueFacetNodes
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iNode
INTEGER :: iFacet
INTEGER :: iPoint
INTEGER :: iVertex
INTEGER :: FacetID
INTEGER :: NodeID
INTEGER :: nFacets
INTEGER :: nNodes
REAL    :: Coords(1:3)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tFacet),POINTER            :: aFacet
TYPE(tFacetNodePtr),ALLOCATABLE :: TessellationNodes(:)
TYPE(tFacetNodePtr),ALLOCATABLE :: FacetNodes(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tBucket),ALLOCATABLE :: Nodes2Facet(:)
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: CalcTimeIni
REAL               :: CalcTimeEnd
CHARACTER(LEN=256) :: ElapsedTime
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

!------------------------------------------------------------!
! FLAGGING ELEMENTS INSIDE GEOMETRY
!------------------------------------------------------------!
SWRITE(UNIT_SCREEN,*)
Header = "IMPORTING GEOMETRY..."
CALL PrintMessage(Header)

CalcTimeIni = RunningTime()

nFacets = SIZE(GeometryData_VerticesCoordinates3D,3)

IF (ALLOCATED(TessellationNodes)) THEN
  DEALLOCATE(TessellationNodes)
END IF
IF (ALLOCATED(FacetNodes)) THEN
  DEALLOCATE(FacetNodes)
END IF
ALLOCATE(TessellationNodes(1:3*nFacets))
ALLOCATE(FacetNodes(1:3))

! Create TessellationNodes pointers
NodeID  = 0
FacetID = 0
DO iFacet=1,nFacets
  FacetID = FacetID+1
  DO iVertex=1,3
    ! Create Pointers to Tessellation Nodes
    NodeID = NodeID+1
    Coords(1:3) = GeometryData_VerticesCoordinates3D(iVertex+1,1:3,iFacet)
    CALL CreateFacetNode(TessellationNodes(NodeID)%Node)
    CALL AddDataToFacetNode(TessellationNodes(NodeID)%Node,NodeID,Coords(1:3))
  END DO
END DO

! Create Facets
NodeID  = 0
FacetID = 0
DO iFacet=1,nFacets
  FacetID = FacetID+1
  DO iVertex=1,3
    NodeID = NodeID+1
    FacetNodes(iVertex)%Node => TessellationNodes(NodeID)%Node
  END DO
  CALL CreateFacet(aFacet,FacetNodes,FacetID)
  CALL AddFacetToList(aFacet,FacetList)
END DO

IF (ALLOCATED(TessellationNodes)) THEN
  DEALLOCATE(TessellationNodes)
END IF
IF (ALLOCATED(FacetNodes)) THEN
  DEALLOCATE(FacetNodes)
END IF

CALL SetUniqueFacetNodes(FacetList)
CALL SetUniqueFacetID(FacetList)

CALL CountFacets(FacetList,nFacets)
CALL CountFacetNodes(FacetList,nNodes)

!------------------------------------------------------------!
! Constructing GeometryData_NodesCoordinates Array
!------------------------------------------------------------!
IF (ALLOCATED(GeometryData_NodesCoordinates)) THEN
  DEALLOCATE(GeometryData_NodesCoordinates)
END IF
ALLOCATE(GeometryData_NodesCoordinates(1:3,1:nNodes))

!------------------------------------------------------------!
! Constructing GeometryData_FacetsToNodes Array
!------------------------------------------------------------!
IF (ALLOCATED(GeometryData_FacetsToNodes)) THEN
  DEALLOCATE(GeometryData_FacetsToNodes)
END IF
ALLOCATE(GeometryData_FacetsToNodes(1:3,1:nFacets))

aFacet => FacetList%FirstFacet
DO WHILE(ASSOCIATED(aFacet))
  DO iVertex=1,3
    FacetID = aFacet%FacetID
    NodeID  = aFacet%Nodes(iVertex)%Node%NodeID
    GeometryData_FacetsToNodes(iVertex,FacetID) = NodeID
    GeometryData_NodesCoordinates(1:3,NodeID)   = aFacet%Nodes(iVertex)%Node%Coords(1:3)
  END DO
  aFacet => aFacet%NextFacet
END DO

!------------------------------------------------------------!
! Constructing Nodes2Facet
!------------------------------------------------------------!
IF (ALLOCATED(GeometryPoints)) THEN
  DEALLOCATE(GeometryPoints)
END IF
ALLOCATE(GeometryPoints(3*nFacets))

iPoint = 1
DO iFacet=1,nFacets
  GeometryPoints(iPoint+0)%PointID = iPoint+0
  GeometryPoints(iPoint+0)%Coords  = GeometryData_VerticesCoordinates3D(2,1:3,iFacet)  
  GeometryPoints(iPoint+1)%PointID = iPoint+1
  GeometryPoints(iPoint+1)%Coords  = GeometryData_VerticesCoordinates3D(3,1:3,iFacet)
  GeometryPoints(iPoint+2)%PointID = iPoint+2
  GeometryPoints(iPoint+2)%Coords  = GeometryData_VerticesCoordinates3D(4,1:3,iFacet)
  iPoint = iPoint+3
END DO

IF (ALLOCATED(GeometryFacets)) THEN
  DEALLOCATE(GeometryFacets)
END IF
ALLOCATE(GeometryFacets(1:nFacets))

DO iFacet=1,nFacets
  GeometryFacets(iFacet)%VerticesCoords(1:3,1:3) = GeometryData_VerticesCoordinates3D(2:4,1:3,iFacet)
END DO

!------------------------------------------------------------!
! Constructing Nodes2Facet
!------------------------------------------------------------!
IF (ALLOCATED(Nodes2Facet)) THEN
  DEALLOCATE(Nodes2Facet)
END IF
ALLOCATE(Nodes2Facet(1:nNodes))

DO iNode=1,nNodes
  CALL CreateBucket(Nodes2Facet(iNode))
END DO

DO iFacet=1,nFacets
  DO iVertex=1,3
    FacetID = iFacet
    NodeID  = GeometryData_FacetsToNodes(iVertex,FacetID)
    CALL AddDataToBucket(Nodes2Facet(NodeID),FacetID)
  END DO
END DO

CALL PrintAnalyze("nFacets",FormatNumber(nFacets))
CALL PrintAnalyze("nNodes",FormatNumber(nNodes))

CalcTimeEnd = RunningTime()
CALL ComputeRuntime(CalcTimeEnd-CalcTimeIni,ElapsedTime)
CALL PrintAnalyze("Elapsed Time",ElapsedTime,nTabIn=2)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ExtractPointsFromGeometryImport
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_GeometryImport
!======================================================================================================================!
