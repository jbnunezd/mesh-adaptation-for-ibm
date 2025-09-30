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
MODULE MOD_StringsHandling
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE LowerCase
  MODULE PROCEDURE LowerCase
END INTERFACE

INTERFACE SplitString
  MODULE PROCEDURE SplitString
END INTERFACE

INTERFACE ReplaceString
  MODULE PROCEDURE ReplaceString
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: LowerCase
PUBLIC :: SplitString
PUBLIC :: ReplaceString
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
END MODULE MOD_StringsHandling
!======================================================================================================================!
