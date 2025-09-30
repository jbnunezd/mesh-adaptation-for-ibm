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
MODULE MOD_ExportToTECPLOT
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE ExportMeshToTECPLOT_ASCII
  MODULE PROCEDURE ExportMesh3DToTECPLOT_ASCII
END INTERFACE

INTERFACE ExportMeshAndDataToTECPLOT_ASCII
  MODULE PROCEDURE ExportMeshAndData3DToTECPLOT_ASCII
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: ExportMeshToTECPLOT_ASCII
PUBLIC :: ExportMeshAndDataToTECPLOT_ASCII
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
SUBROUTINE ExportMesh3DToTECPLOT_ASCII(&
  FileName,ProjectName,ProgramName,FileVersion,nDims,NGeo,nElems,OutputTime,CoordNames,MeshData)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: FileName
CHARACTER(LEN=*),INTENT(IN) :: ProjectName
CHARACTER(LEN=*),INTENT(IN) :: ProgramName
CHARACTER(LEN=*),INTENT(IN) :: FileVersion
INTEGER,         INTENT(IN) :: nDims
INTEGER,         INTENT(IN) :: NGeo
INTEGER,         INTENT(IN) :: nElems
REAL,            INTENT(IN) :: OutputTime
CHARACTER(LEN=*),INTENT(IN) :: CoordNames(1:nDims)
REAL,            INTENT(IN) :: MeshData(1:nDims,0:NGeo,0:NGeo,0:NGeo,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i
INTEGER            :: j
INTEGER            :: k
INTEGER            :: iElem
INTEGER            :: Offset
INTEGER            :: NodeIDElem
INTEGER            :: UNIT_FILE
CHARACTER(LEN=256) :: StrTime
CHARACTER(LEN=256) :: StrNodes
CHARACTER(LEN=256) :: StrElems
CHARACTER(LEN=256) :: FormatString
CHARACTER(LEN=512) :: FormatTitle
CHARACTER(LEN=256) :: FullFileName
CHARACTER(LEN=256) :: FileExtension
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrL
CHARACTER(LEN=256) :: StrR
!----------------------------------------------------------------------------------------------------------------------!

FileExtension = ".dat"
FullFileName  = TRIM(FileName)//TRIM(FileExtension)

StrL = "Writing Mesh"
StrR = TRIM(FullFileName)
CALL PrintAnalyze(StrL,StrR)

FormatTitle = ""
FormatTitle = TRIM(FormatTitle)//"VARIABLES="
FormatTitle = TRIM(FormatTitle)//'"'//TRIM(CoordNames(1))//'",'
FormatTitle = TRIM(FormatTitle)//'"'//TRIM(CoordNames(2))//'",'
FormatTitle = TRIM(FormatTitle)//'"'//TRIM(CoordNames(3))//'"'
Offset = LEN(TRIM(FormatTitle))+1

WRITE(StrTime,'(F15.8)') 0.0
WRITE(StrElems,'(I0)') nElems*(NGeo)**3
WRITE(StrNodes,'(I0)') nElems*(NGeo+1)**3
OPEN(NEWUNIT=UNIT_FILE,FILE=TRIM(FullFileName),STATUS="REPLACE")
WRITE(UNIT_FILE,'(A)') FormatTitle(1:Offset-1)
WRITE(UNIT_FILE,"(17(A))") &
  'ZONE T="', TRIM(ProjectName), '",', &
  'ZONETYPE=FEBRICK', ",", &
  'DATAPACKING=POINT', ",", &
  'STRANDID=1', ",", &
  'SOLUTIONTIME=', TRIM(ADJUSTL(TRIM(StrTime))), ",", &
  'NODES=', TRIM(StrNodes), ",", &
  'ELEMENTS=', TRIM(StrElems)

! Mesh nodes
WRITE(FormatString,'(A,I0,A)') "(", nDims, "(ES19.12E2,1X))"
DO iElem=1,nElems
  DO k=0,NGeo
    DO j=0,NGeo
      DO i=0,NGeo
        WRITE(UNIT_FILE,FormatString) MeshData(1:nDims,i,j,k,iElem)
      END DO
    END DO
  END DO
END DO

! Element MeshConnectivity
FormatString = "(8(I0,1X))"
DO iElem=0,nElems-1
  NodeIDElem=iElem*(NGeo+1)**3
  DO k=1,NGeo
    DO j=1,NGeo
      DO i=1,NGeo
        WRITE(UNIT_FILE,FormatString) &
          NodeIDElem + (i+0) + (j-1)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P1
          NodeIDElem + (i+1) + (j-1)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P2
          NodeIDElem + (i+1) + (j+0)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P3
          NodeIDElem + (i+0) + (j+0)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P4
          NodeIDElem + (i+0) + (j-1)*(NGeo+1) + (k+0)*(NGeo+1)**2, & !P5
          NodeIDElem + (i+1) + (j-1)*(NGeo+1) + (k+0)*(NGeo+1)**2, & !P6
          NodeIDElem + (i+1) + (j+0)*(NGeo+1) + (k+0)*(NGeo+1)**2, & !P7
          NodeIDElem + (i+0) + (j+0)*(NGeo+1) + (k+0)*(NGeo+1)**2    !P8
      END DO
    END DO
  END DO
END DO

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ExportMesh3DToTECPLOT_ASCII
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ExportMeshAndData3DToTECPLOT_ASCII(&
  FileName,ProjectName,ProgramName,FileVersion,nDims,NGeo,nElems,nOutVars,OutputTime,CoordNames,VarNames,MeshData,OutputData)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: FileName
CHARACTER(LEN=*),INTENT(IN) :: ProjectName
CHARACTER(LEN=*),INTENT(IN) :: ProgramName
CHARACTER(LEN=*),INTENT(IN) :: FileVersion
INTEGER,         INTENT(IN) :: nDims
INTEGER,         INTENT(IN) :: NGeo
INTEGER,         INTENT(IN) :: nElems
INTEGER,         INTENT(IN) :: nOutVars
REAL,            INTENT(IN) :: OutputTime
CHARACTER(LEN=*),INTENT(IN) :: CoordNames(1:nDims)
CHARACTER(LEN=*),INTENT(IN) :: VarNames(1:nOutVars)
REAL,            INTENT(IN) :: MeshData(1:nDims,0:NGeo,0:NGeo,0:NGeo,1:nElems)
REAL,            INTENT(IN) :: OutputData(1:nOutVars,0:NGeo,0:NGeo,0:NGeo,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i
INTEGER            :: j
INTEGER            :: k
INTEGER            :: iVar
INTEGER            :: iElem
INTEGER            :: Offset
INTEGER            :: NodeIDElem
INTEGER            :: UNIT_FILE
CHARACTER(LEN=256) :: StrTime
CHARACTER(LEN=256) :: StrNodes
CHARACTER(LEN=256) :: StrElems
CHARACTER(LEN=256) :: VarString
CHARACTER(LEN=256) :: FormatString
CHARACTER(LEN=512) :: FormatTitle
CHARACTER(LEN=256) :: FullFileName
CHARACTER(LEN=256) :: FileExtension
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrL
CHARACTER(LEN=256) :: StrR
!----------------------------------------------------------------------------------------------------------------------!

FileExtension = ".dat"
FullFileName  = TRIM(FileName)//TRIM(FileExtension)

StrL = "Writing Mesh"
StrR = TRIM(FullFileName)
CALL PrintAnalyze(StrL,StrR)

FormatTitle = ""
FormatTitle = TRIM(FormatTitle)//'VARIABLES='
FormatTitle = TRIM(FormatTitle)//'"'//TRIM(CoordNames(1))//'",'
FormatTitle = TRIM(FormatTitle)//'"'//TRIM(CoordNames(2))//'",'
FormatTitle = TRIM(FormatTitle)//'"'//TRIM(CoordNames(3))//'"'
Offset = LEN(TRIM(FormatTitle))+1
DO iVar=1,nOutVars
  WRITE(VarString,'(A2,A,A1)') ',"', TRIM(VarNames(iVar)), '"'
  FormatTitle(Offset:Offset+LEN(TRIM(VarString))) = TRIM(VarString)
  Offset = Offset + LEN(TRIM(VarString))
END DO

WRITE(StrTime,'(F15.8)') 0.0
WRITE(StrElems,'(I8)') nElems*(NGeo)**3
WRITE(StrNodes,'(I8)') nElems*(NGeo+1)**3
OPEN(NEWUNIT=UNIT_FILE,FILE=TRIM(FullFileName),STATUS="REPLACE")
WRITE(UNIT_FILE,'(A)') FormatTitle(1:Offset-1)
WRITE(UNIT_FILE,"(17(A))") &
  'ZONE T="', TRIM(ProjectName), '",', &
  'ZONETYPE=FEBRICK', ",", &
  'DATAPACKING=POINT', ",", &
  'STRANDID=1', ",", &
  'SOLUTIONTIME=', TRIM(ADJUSTL(TRIM(StrTime))), ",", &
  'NODES=', TRIM(ADJUSTL(TRIM(StrNodes))), ",", &
  'ELEMENTS=', TRIM(ADJUSTL(TRIM(StrElems)))

! Mesh nodes and extra data
WRITE(FormatString,'(A,I0,A)') "(", nDims+nOutVars, "(ES19.12E2,1X))"
DO iElem=1,nElems
  DO k=0,NGeo
    DO j=0,NGeo
      DO i=0,NGeo
        WRITE(UNIT_FILE,FormatString) MeshData(1:nDims,i,j,k,iElem), OutputData(1:nOutVars,i,j,k,iElem)
      END DO
    END DO
  END DO
END DO

! Element MeshConnectivity
FormatString = "(8(I0,1X))"
DO iElem=0,nElems-1
  NodeIDElem=iElem*(NGeo+1)**3
  DO k=1,NGeo
    DO j=1,NGeo
      DO i=1,NGeo
        WRITE(UNIT_FILE,FormatString) &
          NodeIDElem + (i+0) + (j-1)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P1
          NodeIDElem + (i+1) + (j-1)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P2
          NodeIDElem + (i+1) + (j+0)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P3
          NodeIDElem + (i+0) + (j+0)*(NGeo+1) + (k-1)*(NGeo+1)**2, & !P4
          NodeIDElem + (i+0) + (j-1)*(NGeo+1) + (k+0)*(NGeo+1)**2, & !P5
          NodeIDElem + (i+1) + (j-1)*(NGeo+1) + (k+0)*(NGeo+1)**2, & !P6
          NodeIDElem + (i+1) + (j+0)*(NGeo+1) + (k+0)*(NGeo+1)**2, & !P7
          NodeIDElem + (i+0) + (j+0)*(NGeo+1) + (k+0)*(NGeo+1)**2    !P8
      END DO
    END DO
  END DO
END DO

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ExportMeshAndData3DToTECPLOT_ASCII
!======================================================================================================================!
!
!
!
!======================================================================================================================!
END MODULE MOD_ExportToTECPLOT
!======================================================================================================================!
