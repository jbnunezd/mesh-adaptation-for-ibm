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
MODULE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL            :: Logging
LOGICAL            :: LogErrors
LOGICAL            :: MPIROOT
REAL               :: StartTime
INTEGER            :: SCREEN_WIDTH
REAL,PARAMETER     :: EPS_MACHINE   = EPSILON(1.0D0)
REAL,PARAMETER     :: PP_ACCURACY   = 1.0E-14
INTEGER,PARAMETER  :: UNIT_SCREEN   = 6
INTEGER,PARAMETER  :: UNIT_INFO     = 111
INTEGER,PARAMETER  :: UNIT_LOGGING  = 222
INTEGER,PARAMETER  :: UNIT_ERRORS   = 333
CHARACTER(LEN=256) :: ErrorFileName = "NOT_SET"
CHARACTER(LEN=256) :: ProgramName   = "MESH-ADAPTER"
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMain
  MODULE PROCEDURE InitializeMain
END INTERFACE

INTERFACE BANNER
  MODULE PROCEDURE BANNER
END INTERFACE

INTERFACE TIMESTAMP
  MODULE PROCEDURE TIMESTAMP
END INTERFACE

INTERFACE ComputeRuntime
  MODULE PROCEDURE ComputeRuntime
END INTERFACE

INTERFACE RunningTime
  MODULE PROCEDURE RunningTime
END INTERFACE

INTERFACE PrintError
  MODULE PROCEDURE PrintError
END INTERFACE

INTERFACE PrintHeader
  MODULE PROCEDURE PrintHeader
END INTERFACE

INTERFACE PrintParameters
  MODULE PROCEDURE PrintParameters
END INTERFACE

INTERFACE PrintMessage
  MODULE PROCEDURE PrintMessage
END INTERFACE

INTERFACE PrintAnalyze
  MODULE PROCEDURE PrintAnalyze
END INTERFACE

INTERFACE GetCurrentTime
  MODULE PROCEDURE GetCurrentTime
END INTERFACE

INTERFACE GetCurrentDate
  MODULE PROCEDURE GetCurrentDate
END INTERFACE

INTERFACE PrintCurrentTime
  MODULE PROCEDURE PrintCurrentTime
END INTERFACE

INTERFACE CreateSeparatingLine
  MODULE PROCEDURE CreateSeparatingLine
END INTERFACE

INTERFACE PrintSeparatingLine
  MODULE PROCEDURE PrintSeparatingLine
END INTERFACE

INTERFACE HighlightText
  MODULE PROCEDURE HighlightText
END INTERFACE

INTERFACE PrintArrayInfo
  MODULE PROCEDURE PrintArrayInfo_INTEGER
  MODULE PROCEDURE PrintArrayInfo_REAL
END INTERFACE

INTERFACE FormatNumber
  MODULE PROCEDURE FormatNumber_INTEGER_rank0
  MODULE PROCEDURE FormatNumber_INTEGER_rank1
  MODULE PROCEDURE FormatNumber_REAL_rank0
  MODULE PROCEDURE FormatNumber_REAL_rank1
END INTERFACE
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
SUBROUTINE InitializeMain()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL TerminalWidth(SCREEN_WIDTH)
CALL BANNER()

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMain
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BANNER()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: ScreenWidth
INTEGER            :: StringWidth
INTEGER            :: xIni
CHARACTER(LEN=256) :: StrFormat
!----------------------------------------------------------------------------------------------------------------------!

ScreenWidth = SCREEN_WIDTH
StringWidth = 53
xIni = MAX((ScreenWidth-StringWidth)/2,1)
SWRITE(StrFormat,'(A1,I4,A4)') "(", xIni, "X,A)"

IF (SCREEN_WIDTH .GT. StringWidth) THEN
  SWRITE(UNIT_SCREEN,*)
  SWRITE(UNIT_SCREEN,StrFormat) " _  _  _    _  _  _    _  _  _    _  _  _    _     _ "
  SWRITE(UNIT_SCREEN,StrFormat) "[_][_][_]  [_][_][_]  [_][_][_]  [_][_][_]  [_]   [_]"
  SWRITE(UNIT_SCREEN,StrFormat) "[_] _ [_]  [_] _ [_]     [_]     [_] _      [_]]_ [_]"
  SWRITE(UNIT_SCREEN,StrFormat) "[_][_][_]  [_][_][_      [_]     [_][_]     [_][_][_]"
  SWRITE(UNIT_SCREEN,StrFormat) "[_]   [_]  [_]   [_]   _ [_] _   [_] _  _   [_]  [[_]"
  SWRITE(UNIT_SCREEN,StrFormat) "[_]   [_]  [_]   [_]  [_][_][_]  [_][_][_]  [_]   [_]"
  SWRITE(UNIT_SCREEN,*)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BANNER
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION TIMESTAMP(Filename,Time)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,            INTENT(IN) :: Time
CHARACTER(LEN=*),INTENT(IN) :: Filename
CHARACTER(LEN=256)          :: TimeStamp
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
!----------------------------------------------------------------------------------------------------------------------!

WRITE(TimeStamp,'(F15.7)') Time
DO i=1,LEN(TRIM(TimeStamp))
  IF(TimeStamp(i:i) .EQ. " ") THEN
    TimeStamp(i:i) = "0"
  END IF
END DO
TimeStamp=TRIM(Filename)//'_'//TRIM(TimeStamp)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION TIMESTAMP
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ComputeRuntime(CalcTime,FormatedTime)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,            INTENT(IN)  :: CalcTime
CHARACTER(LEN=*),INTENT(OUT) :: FormatedTime
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL               :: Time
INTEGER            :: RunDays, RunHours, RunMins, RunSecs
CHARACTER(LEN=256) :: RunSecsTMP
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

Time     = CalcTime
RunDays  = INT(Time/86400.0)
Time     = Time - (RunDays*86400.0)
RunHours = INT(Time/3600.0)
Time     = Time - (RunHours*3600.0)
RunMins  = INT(Time/60.0)
RunSecs  = NINT(Time-(RunMins*60.0))

FormatedTime = ""
IF (RunDays .GT. 0) THEN
  FormatString = "(I2.2,':',I2.2,':',I2.2,':',I2.2,' days:hours:min:sec')"
  SWRITE(FormatedTime,FormatString) RunDays, RunHours, RunMins, RunSecs
ELSEIF (RunHours .GT. 0) THEN
  FormatString = "(I2.2,':',I2.2,':',I2.2,' hours:min:sec')"
  SWRITE(FormatedTime,FormatString) RunHours, RunMins, RunSecs
ELSEIF (RunMins .GT. 0) THEN
  FormatString = "(I2.2,':',I2.2,' min:sec')"
  SWRITE(FormatedTime,FormatString) RunMins, RunSecs
ELSE
  FormatString = "(F6.3,' sec')"
  SWRITE(RunSecsTMP,FormatString) Time
  SWRITE(FormatedTime,"(A)") ADJUSTL(RunSecsTMP)
ENDIF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ComputeRuntime
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION RunningTime()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! ARGUMENT VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL :: RunningTime
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL CPU_TIME(RunningTime)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION RunningTime
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE TerminalWidth(Width)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(OUT) :: Width
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER             :: UNIT_FILE
CHARACTER(LEN=256)  :: PrintLine
CHARACTER(LEN=256)  :: temp
!----------------------------------------------------------------------------------------------------------------------!

PrintLine = "printf " // "%`tput cols`s" // "|tr ' ' '='" // " > TerminalWidth.txt "
CALL EXECUTE_COMMAND_LINE(PrintLine)

OPEN(NEWUNIT=UNIT_FILE,FILE='TerminalWidth.txt',STATUS='OLD',ACTION='READ')
READ(UNIT_FILE,*) temp
CLOSE(UNIT_FILE,STATUS="DELETE")
Width = LEN(TRIM(temp))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE TerminalWidth
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CreateSeparatingLine(Symbol,SeparatingLine)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1),INTENT(IN)               :: Symbol
CHARACTER(LEN=:),ALLOCATABLE,INTENT(OUT)  :: SeparatingLine
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
!----------------------------------------------------------------------------------------------------------------------!

SeparatingLine = ""
DO ii=1,SCREEN_WIDTH
  SeparatingLine = SeparatingLine//TRIM(Symbol)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateSeparatingLine
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintSeparatingLine(Symbol)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1),INTENT(IN) :: Symbol
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=:),ALLOCATABLE :: SeparatingLine
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateSeparatingLine(Symbol,SeparatingLine)
WRITE(UNIT_SCREEN,"(A)") TRIM(SeparatingLine)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintSeparatingLine
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintError(SourceFile,SourceLine,CompDate,CompTime,ErrorMessage)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,         INTENT(IN) :: SourceLine
CHARACTER(LEN=*),INTENT(IN) :: SourceFile
CHARACTER(LEN=*),INTENT(IN) :: CompDate
CHARACTER(LEN=*),INTENT(IN) :: CompTime
CHARACTER(LEN=*),INTENT(IN) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: StrKey
CHARACTER(LEN=256) :: StrMessage
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1)             :: Sep
CHARACTER(LEN=:),ALLOCATABLE :: StrLine1
CHARACTER(LEN=:),ALLOCATABLE :: StrLine2
!----------------------------------------------------------------------------------------------------------------------!

Sep = ":"

CALL CreateSeparatingLine("=",StrLine1)
CALL CreateSeparatingLine("-",StrLine2)

WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,*)
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine1)
!----------------------------------------------------------------------------------------------------------------------!
StrMessage = "FATAL ERROR"
WRITE(UNIT_SCREEN,'(1X,(A))',ADVANCE='YES') TRIM(StrMessage)
!----------------------------------------------------------------------------------------------------------------------!
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine2)
!----------------------------------------------------------------------------------------------------------------------!
StrKey = "Error Message"
StrMessage = TRIM(ErrorMessage)
WRITE(UNIT_SCREEN,'(1X,*(A,T20))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T48))',ADVANCE='YES') TRIM(StrMessage)
!----------------------------------------------------------------------------------------------------------------------!
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine2)
!----------------------------------------------------------------------------------------------------------------------!
StrKey = "File Name"
StrMessage = TRIM(SourceFile)
WRITE(UNIT_SCREEN,'(1X,*(A,T20))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T48))',ADVANCE='YES') TRIM(StrMessage)
!----------------------------------------------------------------------------------------------------------------------!
StrKey = "Line Number"
WRITE(StrMessage,"(I0)") SourceLine
WRITE(UNIT_SCREEN,'(1X,*(A,T20))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T48))',ADVANCE='YES') TRIM(StrMessage)
!----------------------------------------------------------------------------------------------------------------------!
StrKey = "Compilation Date"
StrMessage = TRIM(CompDate)//" at "//TRIM(CompTime)
WRITE(UNIT_SCREEN,'(1X,*(A,T20))',ADVANCE='NO') TRIM(StrKey)
WRITE(UNIT_SCREEN,'(1X,A1)',ADVANCE='NO') TRIM(Sep)
WRITE(UNIT_SCREEN,'(1X,*(A,T48))',ADVANCE='YES') TRIM(StrMessage)
!----------------------------------------------------------------------------------------------------------------------!
WRITE(UNIT_SCREEN,"(A)") TRIM(StrLine1)
WRITE(UNIT_SCREEN,*)

CALL EXIT(1)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintError
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintHeader(Text1,nTabIn)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Text1
INTEGER,INTENT(IN),OPTIONAL :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nTab
CHARACTER(LEN=256) :: TabStr
CHARACTER(LEN=:),ALLOCATABLE :: StrLine
!----------------------------------------------------------------------------------------------------------------------!


IF (PRESENT(nTabIn) .AND. (nTabIn .GE. 0)) THEN
  nTab = nTabIn
ELSE
  nTab = 0
END IF
IF (nTab .EQ. 0) THEN
  WRITE(TabStr,'(A)') "(A)"
ELSE
  WRITE(TabStr,'(A,I0,A)') "(", nTab, "X)"
END IF

CALL CreateSeparatingLine("-",StrLine)

WRITE(UNIT_SCREEN,TabStr,ADVANCE='NO')
WRITE(UNIT_SCREEN,"(A)",ADVANCE='YES') TRIM(StrLine)
WRITE(UNIT_SCREEN,"(A)",ADVANCE='YES') TRIM(Text1)
WRITE(UNIT_SCREEN,"(A)",ADVANCE='YES') TRIM(StrLine)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintHeader
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintParameters(Text1,Text2)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Text1
CHARACTER(LEN=*),INTENT(IN) :: Text2
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=1)   :: Sep1
CHARACTER(LEN=1)   :: Sep2
!----------------------------------------------------------------------------------------------------------------------!

Sep1 = "-"
Sep2 = ":"

WRITE(UNIT_SCREEN,'(*(A,T1,1X))',ADVANCE='NO') Sep1
WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T30))',ADVANCE='NO') ADJUSTL(TRIM(Text1))
WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T1,1X))',ADVANCE='NO') Sep2
WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T48))',ADVANCE='YES') ADJUSTL(TRIM(Text2))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintParameters
!======================================================================================================================!
!
!
!======================================================================================================================!
SUBROUTINE PrintMessage(Text1,Separator,nTabIn)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: Text1
CHARACTER(LEN=1),INTENT(IN),OPTIONAL :: Separator
INTEGER,INTENT(IN),OPTIONAL          :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nTab
CHARACTER(LEN=256) :: TabStr
CHARACTER(LEN=256) :: Message
!----------------------------------------------------------------------------------------------------------------------!

IF (PRESENT(nTabIn) .AND. (nTabIn .GE. 0)) THEN
  nTab = nTabIn
ELSE
  nTab = 0
END IF

IF (PRESENT(Separator)) THEN
  WRITE(Message,'(A,1X,A)') TRIM(Separator), TRIM(Text1)
ELSE
  WRITE(Message,'(A)') TRIM(Text1)
END IF

IF (nTab .EQ. 0) THEN
  WRITE(TabStr,'(A)') "(A)"
ELSE
  WRITE(TabStr,'(A,I0,A)') "(", nTab, "X)"
END IF


WRITE(UNIT_SCREEN,TabStr,ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T48,1X))',ADVANCE='NO') ADJUSTL(TRIM(Message))
WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T1))',ADVANCE='YES')

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintMessage
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintAnalyze(Text1,Text2,nTabIn,KeyWidthIn)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Text1
CHARACTER(LEN=*),INTENT(IN) :: Text2
INTEGER,INTENT(IN),OPTIONAL :: nTabIn
INTEGER,INTENT(IN),OPTIONAL :: KeyWidthIn
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: nTab
INTEGER            :: KeyWidth
CHARACTER(LEN=1)   :: Sep1
CHARACTER(LEN=1)   :: Sep2
CHARACTER(LEN=256) :: TabStr
CHARACTER(LEN=256) :: KeyWidthStr
!----------------------------------------------------------------------------------------------------------------------!

Sep1 = "|"
Sep2 = ":"

IF (PRESENT(nTabIn) .AND. (nTabIn .GE. 0)) THEN
  nTab = nTabIn
ELSE
  nTab = 2
END IF
IF (nTab .EQ. 0) THEN
  WRITE(TabStr,'(A)') "(A)"
ELSE
  WRITE(TabStr,'(A,I0,A)') "(", nTab, "X)"
END IF

KeyWidth = 24
IF (PRESENT(KeyWidthIn) .AND. (KeyWidthIn .GT. KeyWidth)) THEN
  KeyWidth = KeyWidthIn
END IF
KeyWidth = KeyWidth-nTab
WRITE(KeyWidthStr,'(A,I0,A)') "(*(A,T", KeyWidth, "))"

! WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
! WRITE(UNIT_SCREEN,'(*(A,T1))',ADVANCE='NO') Sep1
WRITE(UNIT_SCREEN,TabStr,ADVANCE='NO')
WRITE(UNIT_SCREEN,KeyWidthStr,ADVANCE='NO') ADJUSTL(TRIM(Text1))
WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T1,1X))',ADVANCE='NO') Sep2
WRITE(UNIT_SCREEN,'((1X))',ADVANCE='NO')
WRITE(UNIT_SCREEN,'(*(A,T48))',ADVANCE='YES') ADJUSTL(TRIM(Text2))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintAnalyze
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintCurrentTime()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: CurrentTime
CHARACTER(LEN=256) :: CurrentDate
!----------------------------------------------------------------------------------------------------------------------!

CurrentTime = GetCurrentTime()
CurrentDate = GetCurrentDate()

CALL PrintParameters("System Time",CurrentTime)
CALL PrintParameters("System Date",CurrentDate)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintCurrentTime
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetCurrentTime() RESULT(CurrentTime)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: CurrentTime
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: TimeArray(8)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatStringTime
!----------------------------------------------------------------------------------------------------------------------!

FormatStringTime = "(I2.2,A1,I2.2,A1,I2.2)"
CALL DATE_AND_TIME(values=TimeArray)
WRITE(CurrentTime,FormatStringTime) TimeArray(5), ":", TimeArray(6), ":", TimeArray(7)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetCurrentTime
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION GetCurrentDate() RESULT(CurrentDate)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: CurrentDate
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: TimeArray(8)
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatStringDate
!----------------------------------------------------------------------------------------------------------------------!

FormatStringDate = "(I4.4,A1,I2.2,A1,I2.2)"
CALL DATE_AND_TIME(values=TimeArray)
WRITE(CurrentDate,FormatStringDate) TimeArray(1), "-", TimeArray(2), "-", TimeArray(3)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION GetCurrentDate
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION HighlightText(text,text_style,fg_color,bg_color)
!======================================================================================================================!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN)          :: text
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: text_style
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: fg_color
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: bg_color
CHARACTER(LEN=256)                   :: HighlightText
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=16) :: EscSeqStyle
!----------------------------------------------------------------------------------------------------------------------!

EscSeqStyle = "[0"
IF (PRESENT(text_style)) THEN
  SELECT CASE(text_style)
    CASE('normal')
      EscSeqStyle = TRIM(EscSeqStyle)//";0"
    CASE('bold')
      EscSeqStyle = TRIM(EscSeqStyle)//";1"
    CASE('faint')
      EscSeqStyle = TRIM(EscSeqStyle)//";2"
    CASE('italic')
      EscSeqStyle = TRIM(EscSeqStyle)//";3"
    CASE('underline')
      EscSeqStyle = TRIM(EscSeqStyle)//";4"
    CASE('blink_slow')
      EscSeqStyle = TRIM(EscSeqStyle)//";5"
    CASE('blink_fast')
      EscSeqStyle = TRIM(EscSeqStyle)//";6"
    CASE DEFAULT
      EscSeqStyle = TRIM(EscSeqStyle)//";0"
  END SELECT
ELSE
  EscSeqStyle = TRIM(EscSeqStyle)//";0"
END IF

IF (PRESENT(fg_color)) THEN
  SELECT CASE(fg_color)
    CASE('black')
      EscSeqStyle = TRIM(EscSeqStyle)//";30"
    CASE('red')
      EscSeqStyle = TRIM(EscSeqStyle)//";31"
    CASE('green')
      EscSeqStyle = TRIM(EscSeqStyle)//";32"
    CASE('yellow')
      EscSeqStyle = TRIM(EscSeqStyle)//";33"
    CASE('blue')
      EscSeqStyle = TRIM(EscSeqStyle)//";34"
    CASE('magenta')
      EscSeqStyle = TRIM(EscSeqStyle)//";35"
    CASE('cyan')
      EscSeqStyle = TRIM(EscSeqStyle)//";36"
    CASE('white')
      EscSeqStyle = TRIM(EscSeqStyle)//";37"
    CASE DEFAULT
      EscSeqStyle = TRIM(EscSeqStyle)//";37"
  END SELECT
ELSE
  EscSeqStyle = TRIM(EscSeqStyle)//";39"
END IF

IF (PRESENT(bg_color)) THEN
  SELECT CASE(bg_color)
    CASE('black')
      EscSeqStyle = TRIM(EscSeqStyle)//";40"
    CASE('red')
      EscSeqStyle = TRIM(EscSeqStyle)//";41"
    CASE('green')
      EscSeqStyle = TRIM(EscSeqStyle)//";42"
    CASE('yellow')
      EscSeqStyle = TRIM(EscSeqStyle)//";43"
    CASE('blue')
      EscSeqStyle = TRIM(EscSeqStyle)//";44"
    CASE('magenta')
      EscSeqStyle = TRIM(EscSeqStyle)//";45"
    CASE('cyan')
      EscSeqStyle = TRIM(EscSeqStyle)//";46"
    CASE('white')
      EscSeqStyle = TRIM(EscSeqStyle)//";47"
    CASE DEFAULT
      EscSeqStyle = TRIM(EscSeqStyle)//";47"
  END SELECT
ELSE
  EscSeqStyle = TRIM(EscSeqStyle)//";49"
END IF

HighlightText = CHAR(27)//TRIM(EscSeqStyle)//"m"
HighlightText = TRIM(HighlightText)//TRIM(text)
HighlightText = TRIM(HighlightText)//CHAR(27)//"[m"

!======================================================================================================================!
END FUNCTION HighlightText
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintArrayInfo_INTEGER(Label,VariableNames,NumberIN,nTabIn)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Label
CHARACTER(LEN=*),INTENT(IN) :: VariableNames(:)
INTEGER,INTENT(IN)          :: NumberIN(:)
INTEGER,INTENT(IN),OPTIONAL :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iVar
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Str1
CHARACTER(LEN=256) :: Str2
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VariableNames,1) .NE. SIZE(NumberIN,1)) THEN
  ErrorMessage = "Arrays 'VariableNames' and 'NumberIN' have different size"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

Str1 = ""
Str2 = ""
DO iVar=1,SIZE(VariableNames,1)
  Str1 = "[ "//TRIM(VariableNames(iVar))//" = "//TRIM(FormatNumber(NumberIN(iVar)))//" ]"
  Str2 = TRIM(Str2)//TRIM(Str1)
END DO

IF (PRESENT(nTabIn)) THEN
  CALL PrintAnalyze(Label,Str2,nTabIn)
ELSE
  CALL PrintAnalyze(Label,Str2)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintArrayInfo_INTEGER
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintArrayInfo_REAL(Label,VariableNames,NumberIN,nTabIn)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=*),INTENT(IN) :: Label
CHARACTER(LEN=*),INTENT(IN) :: VariableNames(:)
REAL,INTENT(IN)             :: NumberIN(:)
INTEGER,INTENT(IN),OPTIONAL :: nTabIn
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iVar
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Str1
CHARACTER(LEN=256) :: Str2
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VariableNames,1) .NE. SIZE(NumberIN,1)) THEN
  ErrorMessage = "Arrays 'VariableNames' and 'NumberIN' have different size"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

Str1 = ""
Str2 = ""
DO iVar=1,SIZE(VariableNames,1)
  Str1 = "[ "//TRIM(VariableNames(iVar))//" = "//TRIM(FormatNumber(NumberIN(iVar)))//" ]"
  Str2 = TRIM(Str2)//TRIM(Str1)
END DO

IF (PRESENT(nTabIn)) THEN
  CALL PrintAnalyze(Label,Str2,nTabIn)
ELSE
  CALL PrintAnalyze(Label,Str2)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintArrayInfo_REAL
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION FormatNumber_INTEGER_rank0(NumberIN)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: NumberIN
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: aNumber
INTEGER :: g1e9
INTEGER :: g1e6
INTEGER :: g1e3
INTEGER :: g1ex
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
CHARACTER(LEN=256) :: FormatNumber_INTEGER_rank0
!----------------------------------------------------------------------------------------------------------------------!

aNumber = NumberIN
g1e9    = INT(aNumber/1000000000.0)
aNumber = aNumber - INT(g1e9*1000000000.0)
g1e6    = INT(aNumber/1000000.0)
aNumber = aNumber - INT(g1e6*1000000.0)
g1e3    = INT(aNumber/1000.0)
g1ex    = NINT(aNumber-(g1e3*1000.0))

FormatNumber_INTEGER_rank0 = ""
IF (g1e9 .GT. 0) THEN
  FormatString = "(I0,'.',I3.3,'.',I3.3,'.',I3.3)"
  WRITE(FormatNumber_INTEGER_rank0,FormatString) g1e9, g1e6, g1e3, g1ex
ELSEIF (g1e6 .GT. 0) THEN
  FormatString = "(I0,'.',I3.3,'.',I3.3)"
  WRITE(FormatNumber_INTEGER_rank0,FormatString) g1e6, g1e3, g1ex
ELSEIF (g1e3 .GT. 0) THEN
  FormatString = "(I0,'.',I3.3)"
  WRITE(FormatNumber_INTEGER_rank0,FormatString) g1e3, g1ex
ELSE
  FormatString = "(I0)"
  WRITE(FormatNumber_INTEGER_rank0,FormatString) g1ex
ENDIF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION FormatNumber_INTEGER_rank0
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION FormatNumber_INTEGER_rank1(NumberIN)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: NumberIN(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=512) :: FormatNumber_INTEGER_rank1
!----------------------------------------------------------------------------------------------------------------------!

FormatNumber_INTEGER_rank1 = ""
WRITE(FormatNumber_INTEGER_rank1,"(A)") TRIM(FormatNumber(NumberIN(1)))
DO ii=2,SIZE(NumberIN)
  WRITE(FormatNumber_INTEGER_rank1,"(A,1X,A)") TRIM(FormatNumber_INTEGER_rank1), TRIM(FormatNumber(NumberIN(ii)))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION FormatNumber_INTEGER_rank1
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION FormatNumber_REAL_rank0(NumberIN)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN) :: NumberIN
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
CHARACTER(LEN=256) :: FormatNumber_REAL_rank0
!----------------------------------------------------------------------------------------------------------------------!

FormatNumber_REAL_rank0 = ""
FormatString = "(SP,ES13.6E2)"
WRITE(FormatNumber_REAL_rank0,FormatString) NumberIN

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION FormatNumber_REAL_rank0
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION FormatNumber_REAL_rank1(NumberIN)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN) :: NumberIN(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=512) :: FormatNumber_REAL_rank1
!----------------------------------------------------------------------------------------------------------------------!

FormatNumber_REAL_rank1 = ""
WRITE(FormatNumber_REAL_rank1,"(A)") TRIM(FormatNumber(NumberIN(1)))
DO ii=2,MIN(SIZE(NumberIN),10)
  WRITE(FormatNumber_REAL_rank1,"(A,1X,A)") TRIM(FormatNumber_REAL_rank1), TRIM(FormatNumber(NumberIN(ii)))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION FormatNumber_REAL_rank1
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_GLOBAL_vars
!======================================================================================================================!
