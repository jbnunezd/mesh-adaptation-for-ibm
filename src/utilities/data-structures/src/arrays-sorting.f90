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
MODULE MOD_ArraySorting
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE QuickSort
  MODULE PROCEDURE QuickSort_real_rank1
  MODULE PROCEDURE QuickSort_integer_rank1
END INTERFACE

INTERFACE QuickSortArray
  MODULE PROCEDURE QuickSortArray_real_rank2
  MODULE PROCEDURE QuickSortArray_integer_rank2
END INTERFACE

INTERFACE CheckCondition
  MODULE PROCEDURE CheckCondition_real
  MODULE PROCEDURE CheckCondition_integer
END INTERFACE

INTERFACE SortArray
  MODULE PROCEDURE SortArray_real
  MODULE PROCEDURE SortArray_integer
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE SWAP_real
  MODULE PROCEDURE SWAP_integer
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: QuickSort
PUBLIC :: QuickSortArray
PUBLIC :: SortArray
PUBLIC :: SWAP
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
RECURSIVE SUBROUTINE QuickSort_real_rank1(A)
!----------------------------------------------------------------------------------------------------------------------!
! QuickSort algorithm adapted from:
! http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
!----------------------------------------------------------------------------------------------------------------------!
! Sorting a 1D array with respect to component comp=1
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(INOUT) :: A(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLeft
INTEGER :: iRight
INTEGER :: marker
REAL    :: random
REAL    :: pivot
REAL    :: temp
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(A,1) .LT. 2) THEN
  RETURN
END IF

CALL RANDOM_NUMBER(random)
pivot  = A(INT(random*REAL(SIZE(A,1)-1))+1)
iLeft  = 0
iRight = SIZE(A,1)+1
marker = 0

DO
  iRight = iRight-1
  DO
    IF (A(iRight) .LE. pivot) THEN
      EXIT
    END IF
    iRight = iRight-1
  END DO

  iLeft = iLeft+1
  DO
    IF (A(iLeft) .GE. pivot) THEN
      EXIT
    END IF
    iLeft = iLeft+1
  END DO

  IF (iLeft .LT. iRight) THEN
    temp = A(iLeft)
    A(iLeft)  = A(iRight)
    A(iRight) = temp
  ELSEIF (iLeft .EQ. iRight) THEN
    marker = iLeft+1
    EXIT
  ELSE
    marker = iLeft
    EXIT
  END IF
END DO

CALL QuickSort(A(:marker-1))
CALL QuickSort(A(marker:))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE QuickSort_real_rank1
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE QuickSort_integer_rank1(A)
!----------------------------------------------------------------------------------------------------------------------!
! QuickSort algorithm adapted from:
! http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
!----------------------------------------------------------------------------------------------------------------------!
! Sorting a 1D array with respect to component comp=1
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(INOUT) :: A(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLeft
INTEGER :: iRight
INTEGER :: marker
INTEGER :: pivot
INTEGER :: temp
REAL    :: random
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(A,1) .LT. 2) THEN
  RETURN
END IF

CALL RANDOM_NUMBER(random)
pivot  = A(INT(random*REAL(SIZE(A,1)-1))+1)
iLeft  = 0
iRight = SIZE(A,1)+1
marker = 0

DO
  iRight = iRight-1
  DO
    IF (A(iRight) .LE. pivot) THEN
      EXIT
    END IF
    iRight = iRight-1
  END DO

  iLeft = iLeft+1
  DO
    IF (A(iLeft) .GE. pivot) THEN
      EXIT
    END IF
    iLeft = iLeft+1
  END DO

  IF (iLeft .LT. iRight) THEN
    temp = A(iLeft)
    A(iLeft)  = A(iRight)
    A(iRight) = temp
  ELSEIF (iLeft .EQ. iRight) THEN
    marker = iLeft+1
    EXIT
  ELSE
    marker = iLeft
    EXIT
  END IF
END DO

CALL QuickSort(A(:marker-1))
CALL QuickSort(A(marker:))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE QuickSort_integer_rank1
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE QuickSortArray_real_rank2(A,nCols)
!----------------------------------------------------------------------------------------------------------------------!
! QuickSort algorithm adapted from:
! http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
!----------------------------------------------------------------------------------------------------------------------!
! Sorting a 2D array with respect to components comp=(1:nCols) in rank=2
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(INOUT) :: A(:,:)
INTEGER,INTENT(IN) :: nCols
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLeft
INTEGER :: iRight
INTEGER :: marker
REAL    :: pivot(1:SIZE(A,2))
REAL    :: temp(1:SIZE(A,2))
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(A,1) .LT. 2) THEN
  RETURN
END IF
IF (SIZE(A,2) .LT. nCols) THEN
  ErrorMessage = "dim(A,2) < nCols"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

pivot  = A(SIZE(A,1)/2,1:SIZE(A,2))
iLeft  = 0
iRight = SIZE(A,1)+1
marker = 0

DO
  iRight = iRight-1
  DO
    IF (CheckCondition(A(iRight,:),pivot(:),nCols,+1)) THEN
      EXIT
    END IF
    iRight = iRight-1
  END DO
  iLeft = iLeft+1
  DO
    IF (CheckCondition(A(iLeft,:),pivot(:),nCols,-1)) THEN
      EXIT
    END IF
    iLeft = iLeft+1
  END DO
  IF (iLeft .LT. iRight) THEN
    temp(:) = A(iLeft,:)
    A(iLeft,:)  = A(iRight,:)
    A(iRight,:) = temp(:)
  ELSEIF (iLeft .EQ. iRight) THEN
    marker = iLeft+1
    EXIT
  ELSE
    marker = iLeft
    EXIT
  END IF
END DO

CALL QuickSortArray(A(:marker-1,:),nCols)
CALL QuickSortArray(A(marker:,:),nCols)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE QuickSortArray_real_rank2
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE QuickSortArray_integer_rank2(A,nCols)
!----------------------------------------------------------------------------------------------------------------------!
! QuickSort algorithm adapted from:
! http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
!----------------------------------------------------------------------------------------------------------------------!
! Sorting a 2D array with respect to components comp=(1:nCols) in rank=2
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(INOUT) :: A(:,:)
INTEGER,INTENT(IN)    :: nCols
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iLeft
INTEGER :: iRight
INTEGER :: marker
INTEGER :: pivot(1:SIZE(A,2))
INTEGER :: temp(1:SIZE(A,2))
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(A,1) .LT. 2) THEN
  RETURN
END IF
IF (SIZE(A,2) .LT. nCols) THEN
  ErrorMessage = "dim(A,2) < nCols"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

pivot  = A(SIZE(A,1)/2,1:SIZE(A,2))
iLeft  = 0
iRight = SIZE(A,1)+1
marker = 0

DO
  iRight = iRight-1
  DO
    IF (CheckCondition(A(iRight,:),pivot(:),nCols,+1)) THEN
      EXIT
    END IF
    iRight = iRight-1
  END DO
  iLeft = iLeft+1
  DO
    IF (CheckCondition(A(iLeft,:),pivot(:),nCols,-1)) THEN
      EXIT
    END IF
    iLeft = iLeft+1
  END DO
  IF (iLeft .LT. iRight) THEN
    temp(:) = A(iLeft,:)
    A(iLeft,:)  = A(iRight,:)
    A(iRight,:) = temp(:)
  ELSEIF (iLeft .EQ. iRight) THEN
    marker = iLeft+1
    EXIT
  ELSE
    marker = iLeft
    EXIT
  END IF
END DO

CALL QuickSortArray(A(:marker-1,:),nCols)
CALL QuickSortArray(A(marker:,:),nCols)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE QuickSortArray_integer_rank2
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CheckCondition_real(A,pivot,nCols,Side) RESULT(Status)
!----------------------------------------------------------------------------------------------------------------------!
! QuickSort algorithm adapted from:
! http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
!----------------------------------------------------------------------------------------------------------------------!
! Comparison A(i,:) against pivot(:)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: A(:)
REAL,INTENT(IN)    :: pivot(:)
INTEGER,INTENT(IN) :: nCols
INTEGER,INTENT(IN) :: Side
LOGICAL            :: Status
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(pivot,1) .LT. nCols) THEN
  ErrorMessage = "dim(pivot,1) < nCols"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF
IF (nCols .GT. 5) THEN
  ErrorMessage = "CheckCondition implemented only up to nCols=5"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

Status = .FALSE.

! Left Side
IF (Side .EQ. -1) THEN
  SELECT CASE(nCols)
    CASE(1)
      Status = (A(1) .GE. pivot(1)) 
    CASE(2)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GE. pivot(2))))
    CASE(3)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .GE. pivot(3))))
    CASE(4)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .GT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .GE. pivot(4))))
    CASE(5)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .GT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .GT. pivot(4))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .EQ. pivot(4)) .AND. (A(5) .GE. pivot(5))))
  END SELECT
END IF

! Right Side
IF (Side .EQ. +1) THEN
  SELECT CASE(nCols)
    CASE(1)
      Status = (A(1) .LE. pivot(1))
    CASE(2)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LE. pivot(2))))
    CASE(3)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .LE. pivot(3))))
    CASE(4)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .LT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .LE. pivot(4))))
    CASE(5)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .LT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .LT. pivot(4))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .EQ. pivot(4)) .AND. (A(5) .LE. pivot(5))))
  END SELECT
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CheckCondition_real
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CheckCondition_integer(A,pivot,nCols,Side) RESULT(Status)
!----------------------------------------------------------------------------------------------------------------------!
! QuickSort algorithm adapted from:
! http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
!----------------------------------------------------------------------------------------------------------------------!
! Comparison A(i,:) against pivot(:)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: A(:)
INTEGER,INTENT(IN) :: pivot(:)
INTEGER,INTENT(IN) :: nCols
INTEGER,INTENT(IN) :: Side
LOGICAL            :: Status
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(pivot,1) .LT. nCols) THEN
  ErrorMessage = "dim(pivot,1) < nCols"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF
IF (nCols .GT. 5) THEN
  ErrorMessage = "CheckCondition implemented only up to nCols=5"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

Status = .FALSE.

! Left Side
IF (Side .EQ. -1) THEN
  SELECT CASE(nCols)
    CASE(1)
      Status = (A(1) .GE. pivot(1)) 
    CASE(2)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GE. pivot(2))))
    CASE(3)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .GE. pivot(3))))
    CASE(4)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .GT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .GE. pivot(4))))
    CASE(5)
      Status = ((A(1) .GT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .GT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .GT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .GT. pivot(4))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .EQ. pivot(4)) .AND. (A(5) .GE. pivot(5))))
  END SELECT
END IF

! Right Side
IF (Side .EQ. +1) THEN
  SELECT CASE(nCols)
    CASE(1)
      Status = (A(1) .LE. pivot(1))
    CASE(2)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LE. pivot(2))))
    CASE(3)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .LE. pivot(3))))
    CASE(4)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .LT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .LE. pivot(4))))
    CASE(5)
      Status = ((A(1) .LT. pivot(1)) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .LT. pivot(2))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .LT. pivot(3))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .LT. pivot(4))) .OR. &
               ((A(1) .EQ. pivot(1)) .AND. (A(2) .EQ. pivot(2)) .AND. (A(3) .EQ. pivot(3)) .AND. (A(4) .EQ. pivot(4)) .AND. (A(5) .LE. pivot(5))))
  END SELECT
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CheckCondition_integer
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SortArray_real(A)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(INOUT) :: A(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL    :: temp
INTEGER :: i, j
!----------------------------------------------------------------------------------------------------------------------!

DO i=1,SIZE(A)-1
  DO j=i+1,SIZE(A)
    IF (A(i) .GT. A(j)) THEN
      temp = A(i)
      A(i) = A(j)
      A(j) = temp
    END IF
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SortArray_real
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SortArray_integer(A)
!----------------------------------------------------------------------------------------------------------------------!
! Reorder the elements of in increasing order
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(INOUT) :: A(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: temp
INTEGER :: i, j
!----------------------------------------------------------------------------------------------------------------------!

DO i=1,SIZE(A)-1
  DO j=i+1,SIZE(A)
    IF (A(i) .GT. A(j)) THEN
      temp = A(i)
      A(i) = A(j)
      A(j) = temp
    END IF
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SortArray_integer
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SWAP_real(a,b)
!----------------------------------------------------------------------------------------------------------------------!
! Interchange the contents of a and b
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(INOUT) :: a, b
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL :: t
!----------------------------------------------------------------------------------------------------------------------!

t = b
b = a
a = t

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SWAP_real
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SWAP_integer(a,b)
!----------------------------------------------------------------------------------------------------------------------!
! Interchange the contents of a and b
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(INOUT) :: a, b
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: t
!----------------------------------------------------------------------------------------------------------------------!

t = b
b = a
a = t

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SWAP_integer
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_ArraySorting
!======================================================================================================================!
