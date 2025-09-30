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
MODULE MOD_GeometryImport_STL
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE GeometryImport_STL
  MODULE PROCEDURE GeometryImport_STL
END INTERFACE

INTERFACE GeometryImport_STL_ASCII
  MODULE PROCEDURE GeometryImport_STL_ASCII
END INTERFACE

INTERFACE GeometryImport_STL_BINARY
  MODULE PROCEDURE GeometryImport_STL_BINARY
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: GeometryImport_STL
PUBLIC :: GeometryImport_STL_ASCII
PUBLIC :: GeometryImport_STL_BINARY
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
SUBROUTINE GeometryImport_STL(InputFile,VerticesCoordinates3D,Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
USE MOD_DataStructures,ONLY: SplitString
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)  :: InputFile
REAL,ALLOCATABLE,INTENT(OUT) :: VerticesCoordinates3D(:,:,:)
LOGICAL,INTENT(IN),OPTIONAL  :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iExt
LOGICAL            :: IsFormatted
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

! Mesh File
iExt = INDEX(InputFile,'.',BACK=.TRUE.)

! Check if input file is a STL file
IF(InputFile(iExt+1:iExt+3) .NE. 'stl') THEN
  ErrorMessage = "No STL file provided"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

CALL CheckFileIsFormatted(InputFile,IsFormatted)

IF (IsFormatted .EQV. .FALSE.) THEN
  CALL GeometryImport_STL_BINARY(InputFile,VerticesCoordinates3D,Debug)
ELSE
  CALL GeometryImport_STL_ASCII(InputFile,VerticesCoordinates3D,Debug)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryImport_STL
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GeometryImport_STL_ASCII(InputFile,VerticesCoordinates3D,Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
USE MOD_DataStructures,ONLY: SplitString
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)  :: InputFile
REAL,ALLOCATABLE,INTENT(OUT) :: VerticesCoordinates3D(:,:,:)
LOGICAL,INTENT(IN),OPTIONAL  :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: DebugGeometryImport = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iExt
INTEGER            :: iFacet
INTEGER            :: nFacets
INTEGER            :: UNIT_FILE
INTEGER            :: STAT_FILE
LOGICAL            :: IsFormatted
REAL               :: Vector(1:3)
CHARACTER(LEN=256) :: TextLine
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: StrL
CHARACTER(LEN=:),ALLOCATABLE :: StrR
!----------------------------------------------------------------------------------------------------------------------!

! Mesh File
iExt = INDEX(InputFile,'.',BACK=.TRUE.)

! Check if input file is a STL file
IF(InputFile(iExt+1:iExt+3) .NE. 'stl') THEN
  ErrorMessage = "No STL file provided"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!--------------------------------------------------!
! Opening STL input file (ASCII)
!--------------------------------------------------!
STAT_FILE = 0
OPEN(&
  NEWUNIT = UNIT_FILE,   &
  FILE    = InputFile,   &
  STATUS  = "OLD",       &
  ACTION  = "READ",      &
  ACCESS  = "SEQUENTIAL",&
  IOSTAT  = STAT_FILE)
IF (STAT_FILE .NE. 0) THEN
  ErrorMessage = "Error opening STL file"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

CALL CheckFileIsFormatted(InputFile,IsFormatted)

IF (IsFormatted .EQV. .FALSE.) THEN
  ErrorMessage = "Error opening STL file: File is unformatted"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!--------------------------------------------------!
! Reading Nodes from STL input file
!--------------------------------------------------!
nFacets = 0
STAT_FILE = 0
DO
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
  IF (STAT_FILE .NE. 0) THEN
    EXIT
  END IF
  CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
  IF (LowerCase(TRIM(StrL)) .EQ. "solid") THEN
    CYCLE
  END IF
  CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
  IF (LowerCase(TRIM(StrL)) .EQ. "endsolid") THEN
    EXIT
  END IF
  ! Now we read the facet normal and triangle vertices:
  CALL SplitString(ADJUSTL(TRIM(TextLine))," ",StrL,StrR)
  IF (LowerCase(TRIM(StrL)) .EQ. "facet") THEN
    nFacets = nFacets+1
    CYCLE
  END IF
END DO

! Allocating array for storage of STL data
IF (ALLOCATED(VerticesCoordinates3D)) THEN
  DEALLOCATE(VerticesCoordinates3D)
END IF
ALLOCATE(VerticesCoordinates3D(1:4,1:3,1:nFacets))

STAT_FILE = 0
REWIND(UNIT=UNIT_FILE,IOSTAT=STAT_FILE)
IF (STAT_FILE .NE. 0) THEN
  ErrorMessage = "Error opening STL file"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

iFacet = 0
DO
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
  IF (STAT_FILE .NE. 0) THEN
    EXIT
  END IF
  CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
  IF (LowerCase(TRIM(StrL)) .EQ. "solid") THEN
    ! Here we should save the object name in a variable
    ! ObjectName = TRIM(StrR)
    CYCLE
  END IF
  CALL SplitString(TRIM(TextLine)," ",StrL,StrR)
  IF (LowerCase(TRIM(StrL)) .EQ. "endsolid") THEN
    ! Here we should end the reading of stl file
    EXIT
  END IF
  ! Now we read the facet normal and triangle vertices:
  CALL SplitString(ADJUSTL(TRIM(TextLine))," ",StrL,StrR)
  IF (LowerCase(TRIM(StrL)) .EQ. "facet") THEN
    iFacet = iFacet+1
    CALL SplitString(ADJUSTL(TRIM(StrR))," ",StrL,StrR)
    READ(StrR,*) VerticesCoordinates3D(1,1:3,iFacet)
    CALL GetFacetVertices(UNIT_FILE,VerticesCoordinates3D(2:4,1:3,iFacet))
  END IF
END DO

IF (PRESENT(Debug)) THEN
  DebugGeometryImport = Debug
END IF

IF (DebugGeometryImport .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "==============================="
  WRITE(UNIT_SCREEN,*) "Printing Geometry from STL file"
  WRITE(UNIT_SCREEN,*) "==============================="
  WRITE(UNIT_SCREEN,*)
  DO iFacet=1,nFacets
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Facet   :", TRIM(FormatNumber(iFacet))
    Vector(1:3) = VerticesCoordinates3D(1,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Normal  :", TRIM(FormatNumber(Vector(1:3)))
    Vector(1:3) = VerticesCoordinates3D(2,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Vertex-1:", TRIM(FormatNumber(Vector(1:3)))
    Vector(1:3) = VerticesCoordinates3D(3,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Vertex-2:", TRIM(FormatNumber(Vector(1:3)))
    Vector(1:3) = VerticesCoordinates3D(4,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Vertex-3:", TRIM(FormatNumber(Vector(1:3)))
    WRITE(UNIT_SCREEN,*)
  END DO
END IF

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryImport_STL_ASCII
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GeometryImport_STL_BINARY(InputFile,VerticesCoordinates3D,Debug)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
USE MOD_DataStructures,ONLY: SplitString
!----------------------------------------------------------------------------------------------------------------------!
USE ISO_FORTRAN_ENV,ONLY: INT16
USE ISO_FORTRAN_ENV,ONLY: REAL32
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)  :: InputFile
REAL,ALLOCATABLE,INTENT(OUT) :: VerticesCoordinates3D(:,:,:)
LOGICAL,INTENT(IN),OPTIONAL  :: Debug
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL :: DebugGeometryImport = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iExt
INTEGER            :: iFacet
INTEGER            :: nFacets
INTEGER            :: UNIT_FILE
INTEGER            :: STAT_FILE
LOGICAL            :: IsFormatted
REAL               :: Vector(1:3)
CHARACTER(LEN=80)  :: STLHeader
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!
INTEGER(INT16) :: Padding
REAL(REAL32)   :: Data(1:12)
!----------------------------------------------------------------------------------------------------------------------!

! Mesh File
iExt = INDEX(InputFile,'.',BACK=.TRUE.)

! Check if input file is a STL file
IF(InputFile(iExt+1:iExt+3) .NE. 'stl') THEN
  ErrorMessage = "No STL file provided"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!--------------------------------------------------!
! Opening STL input file (BINARY)
!--------------------------------------------------!
STAT_FILE = 0
OPEN(&
  NEWUNIT = UNIT_FILE,    &
  FILE    = InputFile,    &
  STATUS  = "OLD",        &
  ACTION  = "READ",       &
  ACCESS  = "STREAM",     &
  FORM    = "UNFORMATTED",&
  IOSTAT  = STAT_FILE)
IF (STAT_FILE .NE. 0) THEN
  ErrorMessage = "Error opening STL file"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

CALL CheckFileIsFormatted(InputFile,IsFormatted)

IF (IsFormatted .EQV. .TRUE.) THEN
  ErrorMessage = "Error opening STL file: File is formatted"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!--------------------------------------------------!
! Structure of the contents of the binary STL file
!--------------------------------------------------!
! UINT8[80]   – Header                 - 80 bytes
! UINT32      – Number of triangles    -  4 bytes
! FOREACH triangle
!   REAL32[3] – Normal vector          - 12 bytes
!   REAL32[3] – Vertex 1               - 12 bytes
!   REAL32[3] – Vertex 2               - 12 bytes
!   REAL32[3] – Vertex 3               - 12 bytes
!   UINT16    – Attribute byte count   -  2 bytes
! END
!--------------------------------------------------!

! Reading the Header and nFacets
READ(UNIT_FILE) STLHeader
READ(UNIT_FILE) nFacets

! Allocating array for storage of STL data
IF (ALLOCATED(VerticesCoordinates3D)) THEN
  DEALLOCATE(VerticesCoordinates3D)
END IF
ALLOCATE(VerticesCoordinates3D(1:4,1:3,1:nFacets))

! Reading normal vectors and triangle vertices
DO iFacet=1,nFacets
  READ(UNIT_FILE) Data(1:12)
  READ(UNIT_FILE) Padding
  VerticesCoordinates3D(1:4,1:3,iFacet) = REAL(RESHAPE(Data(1:12),(/4,3/),ORDER=[2,1]))
END DO

IF (PRESENT(Debug)) THEN
  DebugGeometryImport = Debug
END IF

IF (DebugGeometryImport .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*)
  WRITE(UNIT_SCREEN,*) "==============================="
  WRITE(UNIT_SCREEN,*) "Printing Geometry from STL file"
  WRITE(UNIT_SCREEN,*) "==============================="
  WRITE(UNIT_SCREEN,*)
  DO iFacet=1,nFacets
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Facet   :", TRIM(FormatNumber(iFacet))
    Vector(1:3) = VerticesCoordinates3D(1,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Normal  :", TRIM(FormatNumber(Vector(1:3)))
    Vector(1:3) = VerticesCoordinates3D(2,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Vertex-1:", TRIM(FormatNumber(Vector(1:3)))
    Vector(1:3) = VerticesCoordinates3D(3,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Vertex-2:", TRIM(FormatNumber(Vector(1:3)))
    Vector(1:3) = VerticesCoordinates3D(4,1:3,iFacet)
    WRITE(UNIT_SCREEN,"(A,2X,A)") "Vertex-3:", TRIM(FormatNumber(Vector(1:3)))
    WRITE(UNIT_SCREEN,*)
  END DO
END IF

CLOSE(UNIT_FILE)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GeometryImport_STL_BINARY
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetFacetVertices(UNIT_FILE,FacetVertices)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures,ONLY: LowerCase
USE MOD_DataStructures,ONLY: SplitString
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: UNIT_FILE
REAL,INTENT(OUT)   :: FacetVertices(1:3,1:3)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iVertex
INTEGER            :: STAT_FILE
CHARACTER(LEN=256) :: TextLine
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: StrL
CHARACTER(LEN=:),ALLOCATABLE :: StrR
!----------------------------------------------------------------------------------------------------------------------!

! Now we read outer loop and skip
READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine

! Now we read three vertices
DO iVertex=1,3
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine
  CALL SplitString(ADJUSTL(TRIM(TextLine))," ",StrL,StrR)
  READ(StrR,*) FacetVertices(iVertex,1:3)
END DO

! Now we read outer endloop and skip
READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine

! Now we read endfacet and skip
READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) TextLine

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetFacetVertices
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CheckFileIsFormatted(InputFile,IsFormatted)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: InputFile
LOGICAL,INTENT(OUT)         :: IsFormatted
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: UNIT_FILE
INTEGER            :: STAT_FILE
CHARACTER(LEN=1)   :: TextCharacter
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

!--------------------------------------------------!
! Opening STL input file (BINARY)
!--------------------------------------------------!
STAT_FILE = 0
IsFormatted = .TRUE.
OPEN(&
  NEWUNIT = UNIT_FILE,    &
  FILE    = InputFile,    &
  STATUS  = "OLD",        &
  ACTION  = "READ",       &
  ACCESS  = "STREAM",     &
  FORM    = "UNFORMATTED",&
  IOSTAT  = STAT_FILE)
IF (STAT_FILE .NE. 0) THEN
  ErrorMessage = "Error opening STL file"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

DO WHILE((STAT_FILE .EQ. 0) .AND. IsFormatted)
  READ(UNIT_FILE,IOSTAT=STAT_FILE) TextCharacter
  IsFormatted = (IsFormatted .AND. (IACHAR(TextCharacter) .LE. 127))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CheckFileIsFormatted
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_GeometryImport_STL
!======================================================================================================================!
