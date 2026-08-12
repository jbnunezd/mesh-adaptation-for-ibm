!======================================================================================================================!
!
! ARIEN SOLVER
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
MODULE MOD_ExportToGMSH
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: ELEMTYPE_EDGE2  = 1
INTEGER,PARAMETER :: ELEMTYPE_TRI3   = 2
INTEGER,PARAMETER :: ELEMTYPE_QUAD4  = 3
INTEGER,PARAMETER :: ELEMTYPE_TETRA4 = 4
INTEGER,PARAMETER :: ELEMTYPE_HEXA8  = 5
INTEGER,PARAMETER :: ELEMTYPE_PRISM6 = 6
INTEGER,PARAMETER :: ELEMTYPE_PYRA5  = 7
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE ExportMeshToGMSH
  MODULE PROCEDURE ExportMeshToGMSH
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: ExportMeshToGMSH
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
SUBROUTINE ExportMeshToGMSH(&
  FileName,&
  ProjectName,&
  ProgramName,&
  FileVersion,&
  ElementsToNodes,&
  NodesCoordinates,&
  BCFacesToNodes,&
  BCFacesToMark,&
  BoundaryMark,&
  BoundaryName)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: FileName
CHARACTER(LEN=*),INTENT(IN) :: ProjectName
CHARACTER(LEN=*),INTENT(IN) :: ProgramName
CHARACTER(LEN=*),INTENT(IN) :: FileVersion
INTEGER,INTENT(IN)          :: ElementsToNodes(:,:)
REAL,INTENT(IN)             :: NodesCoordinates(:,:)
INTEGER,INTENT(IN)          :: BCFacesToNodes(:,:)
INTEGER,INTENT(IN)          :: BCFacesToMark(:)
INTEGER,INTENT(IN)          :: BoundaryMark(:)
CHARACTER(LEN=*),INTENT(IN) :: BoundaryName(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: UNIT_FILE
INTEGER :: ii
INTEGER :: Tag1
INTEGER :: Tag2
INTEGER :: nTags
INTEGER :: iElem
INTEGER :: iNode
INTEGER :: nNodes
INTEGER :: nElems
INTEGER :: ElemID
INTEGER :: LastElemID
INTEGER :: nElemNodes
INTEGER :: ElemType
INTEGER :: FaceID
INTEGER :: iBCFace
INTEGER :: nBCFaces
INTEGER :: nBoundaries
INTEGER :: nBCFacesNodes
INTEGER :: PhysicalDomain
INTEGER :: PhysicalDimension
INTEGER :: nPhysicalEntities
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=255) :: FormatString
CHARACTER(LEN=255) :: FullFileName
CHARACTER(LEN=255) :: FileExtension
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=255) :: StrL
CHARACTER(LEN=255) :: StrR
!----------------------------------------------------------------------------------------------------------------------!

FileExtension = ".msh"
FullFileName  = TRIM(FileName)//TRIM(FileExtension)

nElems      = SIZE(ElementsToNodes,2)
nNodes      = SIZE(NodesCoordinates,2)
nBCFaces    = SIZE(BCFacesToNodes,2)
nBoundaries = SIZE(BoundaryName,1)

nPhysicalEntities = nBoundaries+1

StrL = "Writing MESH"
StrR = TRIM(FullFileName)
CALL PrintAnalyze(StrL,StrR)

OPEN(NEWUNIT=UNIT_FILE,FILE=TRIM(FullFileName),STATUS="REPLACE")

!--------------------------------------------------!
! SECTION: $MeshFormat
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$MeshFormat"
WRITE(UNIT_FILE,"(A)") "2.2 0 8"
WRITE(UNIT_FILE,"(A)") "$EndMeshFormat"

!--------------------------------------------------!
! SECTION: $PhysicalNames
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$PhysicalNames"
WRITE(UNIT_FILE,"(I0)") nPhysicalEntities

PhysicalDimension = 2
DO ii=1,nBoundaries
  WRITE(UNIT_FILE,"(I0,1X,I0,1X,A1,A,A1)") PhysicalDimension, BoundaryMark(ii), '"', TRIM(BoundaryName(ii)), '"'
END DO
PhysicalDomain = BoundaryMark(nBoundaries)+1
WRITE(UNIT_FILE,"(I0,1X,I0,1X,A1,A,A1)") PhysicalDimension+1, PhysicalDomain, '"', "PhysicalDomain", '"'
WRITE(UNIT_FILE,"(A)") "$EndPhysicalNames"

!--------------------------------------------------!
! SECTION: $Nodes
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$Nodes"
WRITE(UNIT_FILE,"(I0)") nNodes

WRITE(FormatString,'(A)') "(I0,SP,3(1X,ES20.13E2))"
DO iNode=1,nNodes
  WRITE(UNIT_FILE,FormatString) iNode, NodesCoordinates(1:3,iNode)
END DO

WRITE(UNIT_FILE,"(A)") "$EndNodes"

!--------------------------------------------------!
! SECTION: $Elements
!--------------------------------------------------!
WRITE(UNIT_FILE,"(A)") "$Elements"
WRITE(UNIT_FILE,"(I0)") nElems+nBCFaces

nTags = 2
!--------------------------------------------------!
! Tag1: Physical Entity
! Tag2: Geometrical Entity
!--------------------------------------------------!

LastElemID = 0
!--------------------------------------------------!
! ElemType = ELEMTYPE_QUAD4
!--------------------------------------------------!
ElemType = ELEMTYPE_QUAD4
nBCFacesNodes = 4
! Writing Elements-to-Nodes data
WRITE(FormatString,'(A,I0,A)') "(", nBCFacesNodes+5, "(I0,1X))"
FaceID = LastElemID
DO iBCFace=1,nBCFaces
  FaceID = FaceID+1
  Tag1 = BCFacesToMark(iBCFace)
  Tag2 = 1
  WRITE(UNIT_FILE,FormatString) &
    FaceID, ElemType, nTags, Tag1, Tag2, BCFacesToNodes(1:nBCFacesNodes,iBCFace)
END DO
LastElemID = FaceID

!--------------------------------------------------!
! ElemType = ELEMTYPE_HEXA8
!--------------------------------------------------!
ElemType  = ELEMTYPE_HEXA8
nElemNodes = 8

! Writing Elements-to-Nodes data
WRITE(FormatString,'(A,I0,A)') "(", nElemNodes+5, "(I0,1X))"
ElemID = LastElemID
DO iElem=1,nElems
  ElemID = ElemID+1
  Tag1 = PhysicalDomain
  Tag2 = 2
  WRITE(UNIT_FILE,FormatString) &
    ElemID, ElemType, nTags, Tag1, Tag2, ElementsToNodes(1:nElemNodes,iElem)
END DO

WRITE(UNIT_FILE,"(A)") "$EndElements"

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ExportMeshToGMSH
!======================================================================================================================!
!
!
!
!======================================================================================================================!
END MODULE MOD_ExportToGMSH
!======================================================================================================================!
