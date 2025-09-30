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
MODULE MOD_ConfigFilesTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE ReadParameterFile
  MODULE PROCEDURE ReadParameterFile
END INTERFACE

INTERFACE IgnoredStrings
  MODULE PROCEDURE IgnoredStrings
END INTERFACE

INTERFACE GetInteger
  MODULE PROCEDURE GetInteger
END INTERFACE

INTERFACE GetReal
  MODULE PROCEDURE GetReal
END INTERFACE

INTERFACE GetLogical
  MODULE PROCEDURE GetLogical
END INTERFACE

INTERFACE GetString
  MODULE PROCEDURE GetString
END INTERFACE

INTERFACE GetIntegerArray
  MODULE PROCEDURE GetIntegerArray
END INTERFACE

INTERFACE GetRealArray
  MODULE PROCEDURE GetRealArray
END INTERFACE

INTERFACE CountStrings
  MODULE PROCEDURE CountStrings
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tString
  CHARACTER(LEN=:),ALLOCATABLE :: String
  TYPE(tString),POINTER        :: NextString
  TYPE(tString),POINTER        :: PrevString
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tStringList
  TYPE(tString),POINTER :: FirstString
  TYPE(tString),POINTER :: LastString
  CONTAINS
  PROCEDURE :: Construct    => Construct_StringList
  PROCEDURE :: Destruct     => Destruct_StringList
  PROCEDURE :: AddRecord    => AddRecord_StringList
  PROCEDURE :: RemoveRecord => RemoveRecord_StringList
  PROCEDURE :: GetData      => GetData_StringList
  PROCEDURE :: MoveToNext   => MoveToNext_StringList
  PROCEDURE :: PrintList    => PrintList_StringList
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tStringList) :: StringList
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL,PUBLIC :: ReadInDone = .FALSE.
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: ReadParameterFile
PUBLIC :: IgnoredStrings
PUBLIC :: GetInteger
PUBLIC :: GetReal
PUBLIC :: GetLogical
PUBLIC :: GetString
PUBLIC :: GetIntegerArray
PUBLIC :: GetRealArray
PUBLIC :: CountStrings
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
SUBROUTINE ReadParameterFile()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: StrL
CHARACTER(LEN=:),ALLOCATABLE :: StrR
CHARACTER(LEN=:),ALLOCATABLE :: StringLine
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: iExt
INTEGER            :: STAT_FILE
INTEGER            :: UNIT_FILE
CHARACTER(LEN=256) :: Line
CHARACTER(LEN=256) :: Header
CHARACTER(LEN=256) :: InputFile
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (ReadInDone .EQV. .TRUE.) THEN
  RETURN
END IF
ReadInDone = .TRUE.

Header = "READING PARAMETER FILE..."
CALL PrintHeader(Header)

! Parameter File
CALL GET_COMMAND_ARGUMENT(1,InputFile)
iExt = INDEX(InputFile,'.',BACK=.TRUE.)

! Check if input file is a parameter file
IF(InputFile(iExt+1:iExt+3) .NE. 'ini') THEN
  ErrorMessage = "No parameter file given"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

CALL PrintParameters("Parameter File",InputFile)

STAT_FILE = 0
OPEN(NEWUNIT = UNIT_FILE,   &
     FILE    = InputFile,   &
     STATUS  = "OLD",       &
     ACTION  = "READ",      &
     ACCESS  = "SEQUENTIAL",&
     IOSTAT  = STAT_FILE)
IF (STAT_FILE .NE. 0) THEN
  ErrorMessage = "Error when opening parameter file"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

STAT_FILE = 0
CALL StringList%Construct()
DO
  READ(UNIT_FILE,'(A)',IOSTAT=STAT_FILE) Line
  IF (STAT_FILE .NE. 0) THEN
    EXIT
  END IF
  StringLine = Line
  IF (LEN(TRIM(StringLine)) .EQ. 0) THEN
    CYCLE
  END IF
  CALL ReplaceString(TRIM(StringLine)," ","",StringLine)
  CALL SplitString(StringLine,"!",StrL,StrR)
  IF (LEN(TRIM(StrL)) .EQ. 0) THEN
    CYCLE
  ELSE
    StringLine = StrL
  END IF
  CALL SplitString(StringLine,"#",StrL,StrR)
  IF (LEN(TRIM(StrL)) .EQ. 0) THEN
    CYCLE
  ELSE
    StringLine = StrL
  END IF
  IF (LEN(TRIM(StringLine)) .GT. 2) THEN
    CALL StringList%AddRecord(StringLine)
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ReadParameterFile
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindString(Key,KeyValue,DefaultValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)              :: Key
CHARACTER(LEN=*),INTENT(IN),OPTIONAL     :: DefaultValue
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: KeyValue
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: StrL
CHARACTER(LEN=:),ALLOCATABLE :: StrR
CHARACTER(LEN=:),ALLOCATABLE :: TempKey1
CHARACTER(LEN=:),ALLOCATABLE :: TempKey2
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tString),POINTER :: aString
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL            :: Found
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

Found = .FALSE.
aString => StringList%FirstString
DO WHILE(.NOT. Found)
  IF (.NOT. ASSOCIATED(aString)) THEN
    IF (.NOT. PRESENT(DefaultValue)) THEN
      ErrorMessage = "INI-File missing necessary keyword: "//TRIM(Key)
      CALL PrintError(__STAMP__,ErrorMessage)
    ELSE
      KeyValue = TRIM(DefaultValue)
      RETURN
    END IF
  END IF
  CALL SplitString(TRIM(aString%String),"=",StrL,StrR)
  TempKey1 = LowerCase(StrL)
  TempKey2 = LowerCase(Key)
  IF (TRIM(TempKey1) .EQ. TRIM(TempKey2)) THEN
    Found = .TRUE.
    KeyValue = StrR
    CALL StringList%RemoveRecord(aString)
  ELSE
    CALL StringList%MoveToNext(aString)
  END IF  
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindString
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE IgnoredStrings()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: String
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tString),POINTER :: aString
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: HeaderMessage
!----------------------------------------------------------------------------------------------------------------------!

aString => StringList%FirstString
IF (ASSOCIATED(aString)) THEN
  HeaderMessage = "THE FOLLOWING INI-FILE PARAMETERS WERE IGNORED:"
  CALL PrintHeader(TRIM(HeaderMessage))
  DO WHILE(ASSOCIATED(aString))
    CALL StringList%GetData(aString,String)
    CALL PrintMessage(TRIM(String),"-",nTabIn=1)
    CALL StringList%MoveToNext(aString)
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE IgnoredStrings
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetInteger(Key,DefaultValue) RESULT(KeyValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Key
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DefaultValue
INTEGER                              :: KeyValue
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: KeyValue1
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Key1
CHARACTER(LEN=256) :: Key2
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. PRESENT(DefaultValue)) THEN
  CALL FindString(Key,KeyValue1)
ELSE
  CALL FindString(Key,KeyValue1,DefaultValue)
END IF

READ(KeyValue1,*) KeyValue
WRITE(Key2,"(I8)") KeyValue

Key1 = Key
Key2 = TRIM(ADJUSTL(TRIM(Key2)))

CALL PrintParameters(Key1,Key2)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetInteger
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetReal(Key,DefaultValue) RESULT(KeyValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Key
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DefaultValue
REAL                                 :: KeyValue
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: KeyValue1
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Key1
CHARACTER(LEN=256) :: Key2
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. PRESENT(DefaultValue)) THEN
  CALL FindString(Key,KeyValue1)
ELSE
  CALL FindString(Key,KeyValue1,DefaultValue)
END IF

READ(KeyValue1,*) KeyValue
WRITE(Key2,"(ES12.4)") KeyValue

Key1 = Key
Key2 = TRIM(ADJUSTL(TRIM(Key2)))

CALL PrintParameters(Key1,Key2)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetReal
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetLogical(Key,DefaultValue) RESULT(KeyValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Key
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DefaultValue
LOGICAL                              :: KeyValue
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: KeyValue1
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Key1
CHARACTER(LEN=256) :: Key2
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. PRESENT(DefaultValue)) THEN
  CALL FindString(Key,KeyValue1)
ELSE
  CALL FindString(Key,KeyValue1,DefaultValue)
END IF

READ(KeyValue1,*) KeyValue
WRITE(Key2,"(L1)") KeyValue

Key1 = Key
Key2 = TRIM(ADJUSTL(TRIM(Key2)))

CALL PrintParameters(Key1,Key2)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetLogical
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetString(Key,DefaultValue) RESULT(KeyValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Key
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DefaultValue
CHARACTER(LEN=256)                   :: KeyValue
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: KeyValue1
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Key1
CHARACTER(LEN=256) :: Key2
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. PRESENT(DefaultValue)) THEN
  CALL FindString(Key,KeyValue1)
ELSE
  CALL FindString(Key,KeyValue1,DefaultValue)
END IF

READ(KeyValue1,*) KeyValue
WRITE(Key2,"(A)") KeyValue

Key1 = Key
Key2 = TRIM(ADJUSTL(TRIM(Key2)))

CALL PrintParameters(Key1,Key2)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetString
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetIntegerArray(Key,n,DefaultValue) RESULT(KeyValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Key
INTEGER,INTENT(IN)                   :: n
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DefaultValue
INTEGER                              :: KeyValue(1:n)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: KeyValue1
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrKey
CHARACTER(LEN=256) :: StrValue
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. PRESENT(DefaultValue)) THEN
  CALL FindString(Key,KeyValue1)
ELSE
  CALL FindString(Key,KeyValue1,DefaultValue)
END IF

StrKey = Key
StrValue = TRIM(KeyValue1)
StrValue = TRIM(ADJUSTL(TRIM(StrValue)))

CALL ReplaceString(TRIM(KeyValue1),","," ",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"]","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"[","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"(/","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"/)","",KeyValue1) 
CALL ReplaceString(TRIM(KeyValue1),"(","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),")","",KeyValue1) 
  
READ(KeyValue1,*) KeyValue

CALL PrintParameters(StrKey,StrValue)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetIntegerArray
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetRealArray(Key,n,DefaultValue) RESULT(KeyValue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Key
INTEGER,INTENT(IN)                   :: n
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: DefaultValue
REAL                                 :: KeyValue(1:n)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: KeyValue1
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrKey
CHARACTER(LEN=256) :: StrValue
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. PRESENT(DefaultValue)) THEN
  CALL FindString(Key,KeyValue1)
ELSE
  CALL FindString(Key,KeyValue1,DefaultValue)
END IF

StrKey = Key
StrValue = TRIM(KeyValue1)
StrValue = TRIM(ADJUSTL(TRIM(StrValue)))

CALL ReplaceString(TRIM(KeyValue1),","," ",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"]","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"[","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"(/","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),"/)","",KeyValue1) 
CALL ReplaceString(TRIM(KeyValue1),"(","",KeyValue1)
CALL ReplaceString(TRIM(KeyValue1),")","",KeyValue1) 
  
READ(KeyValue1,*) KeyValue

CALL PrintParameters(StrKey,StrValue)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetRealArray
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CountStrings(Key,DefaultValue) RESULT(nStrings)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Key
INTEGER,INTENT(IN),OPTIONAL :: DefaultValue
INTEGER                     :: nStrings
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: StrL
CHARACTER(LEN=:),ALLOCATABLE :: StrR
CHARACTER(LEN=:),ALLOCATABLE :: TempKey1
CHARACTER(LEN=:),ALLOCATABLE :: TempKey2
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tString),POINTER :: aString
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

nStrings = 0
aString => StringList%FirstString
DO WHILE (ASSOCIATED(aString))
  CALL SplitString(TRIM(aString%String),"=",StrL,StrR)
  TempKey1 = LowerCase(StrL)
  TempKey2 = LowerCase(Key)
  IF (TRIM(TempKey1) .EQ. TRIM(TempKey2)) THEN
    nStrings = nStrings+1
  END IF
  CALL StringList%MoveToNext(aString)
END DO

IF (nStrings .EQ. 0) THEN
  IF (PRESENT(DefaultValue)) THEN
    nStrings = DefaultValue
  ELSE
    ErrorMessage = "Inifile missing necessary keyword item: "//TRIM(Key)
    CALL PrintError(__STAMP__,ErrorMessage)
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CountStrings
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Construct_StringList(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

NULLIFY(self%FirstString)
NULLIFY(self%LastString)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Construct_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE AddRecord_StringList(self,String)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT) :: self
CHARACTER(LEN=:),ALLOCATABLE,INTENT(IN) :: String
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tString),POINTER :: aString
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(self%LastString)) THEN
  ALLOCATE(self%FirstString)
  self%LastString => self%FirstString
  self%LastString%String = String
ELSE
  aString => self%LastString
  ALLOCATE(self%LastString)
  self%LastString%String = String
  self%LastString%PrevString => aString
  self%LastString%PrevString%NextString => self%LastString
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE AddRecord_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE RemoveRecord_StringList(self,aString)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT) :: self
TYPE(tString),POINTER,INTENT(IN) :: aString
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(aString%PrevString)) THEN
  self%FirstString => aString%NextString
ELSE
  aString%PrevString%NextString => aString%NextString
END IF

IF (.NOT. ASSOCIATED(aString%NextString)) THEN
  self%LastString => aString%PrevString
ELSE
  aString%NextString%PrevString => aString%PrevString
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE RemoveRecord_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetData_StringList(self,aString,String)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT)         :: self
TYPE(tString),POINTER,INTENT(IN)         :: aString
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: String
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

String = aString%String

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetData_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MoveToNext_StringList(self,aString)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT)    :: self
TYPE(tString),POINTER,INTENT(INOUT) :: aString
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

aString => aString%NextString

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MoveToNext_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintList_StringList(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tString),POINTER :: aString
CHARACTER(LEN=:),ALLOCATABLE :: String
!----------------------------------------------------------------------------------------------------------------------!

aString => self%FirstString
DO WHILE(ASSOCIATED(aString))
  CALL self%GetData(aString,String)
  WRITE(UNIT_SCREEN,'(A)') String
  CALL self%MoveToNext(aString)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintList_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Destruct_StringList(self)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CLASS(tStringList),INTENT(INOUT) :: self
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tString),POINTER :: aString
TYPE(tString),POINTER :: iString
!----------------------------------------------------------------------------------------------------------------------!

aString => self%FirstString
DO WHILE(ASSOCIATED(aString))
  iString => aString%NextString
  DEALLOCATE(aString)
  aString => iString
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Destruct_StringList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION LowerCase(StrIn) RESULT(StrOut)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)  :: StrIn
CHARACTER(LEN=:),ALLOCATABLE :: StrOut
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
CHARACTER(LEN=*),PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: iPos
!----------------------------------------------------------------------------------------------------------------------!

StrOut = StrIn
DO ii=1,LEN(StrOut)
  iPos = INDEX(UPPER_CASE,StrIn(ii:ii))
  IF (iPos .NE. 0) THEN
    StrOut(ii:ii) = LOWER_CASE(iPos:iPos)
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION LowerCase
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SplitString(StrIn,StrSep,StrL,StrR)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)              :: StrIn
CHARACTER(LEN=*),INTENT(IN)              :: StrSep
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: StrL
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: StrR
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iPos
INTEGER :: StrInLen
INTEGER :: StrSepLen
!----------------------------------------------------------------------------------------------------------------------!

StrInLen  = LEN(StrIn)
StrSepLen = LEN(StrSep)

IF (StrInLen .EQ. 0) THEN
  StrL = ""
  StrR = ""
  RETURN
END IF
IF (StrSepLen .EQ. 0) THEN
  StrL = StrIn
  StrR = ""
  RETURN
END IF

iPos = INDEX(StrIn,StrSep)
IF (iPos .EQ. 0) THEN
  StrL = StrIn
  StrR = ""
  RETURN
END IF

StrL = StrIn(1:iPos-1)
StrR = StrIn(iPos+1:StrInLen)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SplitString
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ReplaceString(StrIn,StrSearch,StrReplace,StrOut)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)              :: StrIn
CHARACTER(LEN=*),INTENT(IN)              :: StrSearch
CHARACTER(LEN=*),INTENT(IN)              :: StrReplace
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT) :: StrOut
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: StrInLen
INTEGER :: StrOutLen
INTEGER :: StrSearchLen
INTEGER :: StrReplaceLen
!----------------------------------------------------------------------------------------------------------------------!

StrInLen      = LEN(StrIn)
StrSearchLen  = LEN(StrSearch)
StrReplaceLen = LEN(StrReplace)

IF (StrInLen .EQ. 0) THEN
  StrOut = ""
  RETURN
END IF
IF (StrSearchLen .EQ. 0) THEN
  StrOut = ""
  RETURN
END IF
IF (StrInLen .LT. StrSearchLen) THEN
  StrOut = StrIn
  RETURN
END IF

ii = 1
StrOut = StrIn
DO
  StrOutLen = LEN(StrOut)
  IF (StrOut(ii:ii+StrSearchLen-1) .EQ. StrSearch) THEN
    IF (ii+StrSearchLen .GT. StrOutLen) THEN
      StrOut = StrOut(1:ii-1)//StrReplace
    ELSE
      StrOut = StrOut(1:ii-1)//StrReplace//StrOut(ii+StrSearchLen:StrOutLen)
    END IF    
    IF (ii+StrSearchLen .GT. StrOutLen) THEN
      EXIT
    END IF
    IF (StrReplaceLen .EQ. 0) THEN
      ii = ii-1
    END IF
  END IF
  StrOutLen = LEN(StrOut)
  IF (ii+StrSearchLen .GT. StrOutLen) THEN
    EXIT
  END IF
  ii = ii+1
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ReplaceString
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_ConfigFilesTools
!======================================================================================================================!
