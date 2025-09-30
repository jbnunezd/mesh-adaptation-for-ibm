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
MODULE MOD_LinkedLists
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE :: tArrayREAL
  REAL,ALLOCATABLE :: Data(:)
END TYPE
TYPE :: tArrayINTEGER
  INTEGER,ALLOCATABLE :: Data(:)
END TYPE
TYPE :: tLinkedListNode
  INTEGER :: BucketID
  CLASS(*),ALLOCATABLE :: Bucket
  CLASS(tLinkedListNode),POINTER :: PrevLinkedListNode
  CLASS(tLinkedListNode),POINTER :: NextLinkedListNode
END TYPE
TYPE :: tLinkedList
  CLASS(tLinkedListNode),POINTER :: FirstLinkedListNode => NULL()
  CLASS(tLinkedListNode),POINTER :: LastLinkedListNode  => NULL()
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
!
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateLinkedListNode
  MODULE PROCEDURE CreateLinkedListNode
END INTERFACE

INTERFACE AddLinkedListNode
  MODULE PROCEDURE AddLinkedListNode
END INTERFACE

INTERFACE GetLinkedListNode
  MODULE PROCEDURE GetLinkedListNode
END INTERFACE

INTERFACE PrintLinkedList
  MODULE PROCEDURE PrintLinkedList
END INTERFACE

INTERFACE DestructLinkedList
  MODULE PROCEDURE DestructLinkedList
END INTERFACE

INTERFACE CountLinkedListNodes
  MODULE PROCEDURE CountLinkedListNodes
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
SUBROUTINE CreateLinkedListNode(LinkedListNode,BucketID,Bucket)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedListNode),POINTER,INTENT(INOUT) :: LinkedListNode
INTEGER,INTENT(IN)                          :: BucketID
CLASS(*),INTENT(IN),OPTIONAL                :: Bucket
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(LinkedListNode)

NULLIFY(LinkedListNode%PrevLinkedListNode)
NULLIFY(LinkedListNode%NextLinkedListNode)

LinkedListNode%BucketID = BucketID

IF (PRESENT(Bucket)) THEN
  SELECT TYPE(Bucket)
    TYPE IS(REAL)
      ALLOCATE(LinkedListNode%Bucket,SOURCE=Bucket)
      LinkedListNode%Bucket   = Bucket
    TYPE IS(INTEGER)
      ALLOCATE(LinkedListNode%Bucket,SOURCE=Bucket)
      LinkedListNode%Bucket   = Bucket
    TYPE IS(tArrayREAL)
      ALLOCATE(LinkedListNode%Bucket,SOURCE=Bucket)
      LinkedListNode%Bucket   = Bucket
    TYPE IS(tArrayINTEGER)
      ALLOCATE(LinkedListNode%Bucket,SOURCE=Bucket)
      LinkedListNode%Bucket   = Bucket
  END SELECT
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateLinkedListNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE AddLinkedListNode(LinkedListNode,LinkedList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedListNode),POINTER,INTENT(INOUT) :: LinkedListNode
TYPE(tLinkedList),INTENT(INOUT)             :: LinkedList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

LinkedListNode%PrevLinkedListNode => LinkedList%LastLinkedListNode
NULLIFY(LinkedListNode%NextLinkedListNode)

IF (.NOT. ASSOCIATED(LinkedList%LastLinkedListNode)) THEN
  LinkedList%FirstLinkedListNode => LinkedListNode
  LinkedList%LastLinkedListNode  => LinkedListNode
ELSE
  LinkedList%LastLinkedListNode%NextLinkedListNode => LinkedListNode
END IF

LinkedList%LastLinkedListNode => LinkedListNode

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE AddLinkedListNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE GetLinkedListNode(LinkedListNode,BucketID,Bucket)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedListNode),POINTER,INTENT(INOUT) :: LinkedListNode
INTEGER,INTENT(OUT)                         :: BucketID
CLASS(*),INTENT(OUT),OPTIONAL               :: Bucket
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

BucketID = LinkedListNode%BucketID

IF (PRESENT(Bucket)) THEN
  SELECT TYPE(aBucket => LinkedListNode%Bucket)
    TYPE IS(REAL)
      SELECT TYPE(Bucket)
        TYPE IS(REAL)
          Bucket = aBucket
      END SELECT
    TYPE IS(INTEGER)
      SELECT TYPE(Bucket)
        TYPE IS(INTEGER)
          Bucket = aBucket
      END SELECT
    TYPE IS(tArrayREAL)
      SELECT TYPE(Bucket)
        TYPE IS(tArrayREAL)
          Bucket = aBucket
      END SELECT
    TYPE IS(tArrayINTEGER)
      SELECT TYPE(Bucket)
        TYPE IS(tArrayINTEGER)
          Bucket = aBucket
      END SELECT
  END SELECT
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE GetLinkedListNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DestructLinkedList(LinkedList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList),INTENT(INOUT) :: LinkedList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedListNode),POINTER :: aLinkedListNode
TYPE(tLinkedListNode),POINTER :: bLinkedListNode
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(LinkedList%FirstLinkedListNode)) THEN
  RETURN
END IF

aLinkedListNode => LinkedList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aLinkedListNode))
  bLinkedListNode => aLinkedListNode%NextLinkedListNode
  NULLIFY(aLinkedListNode%PrevLinkedListNode)
  NULLIFY(aLinkedListNode%NextLinkedListNode)
  IF (ALLOCATED(aLinkedListNode%Bucket)) THEN
    DEALLOCATE(aLinkedListNode%Bucket)
  END IF
  DEALLOCATE(aLinkedListNode)
  aLinkedListNode => bLinkedListNode
END DO

NULLIFY(LinkedList%FirstLinkedListNode)
NULLIFY(LinkedList%LastLinkedListNode)
  
!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DestructLinkedList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintLinkedList(LinkedList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList),INTENT(INOUT) :: LinkedList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nData
INTEGER :: iBucket
INTEGER :: BucketID
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedListNode),POINTER :: aLinkedListNode
!----------------------------------------------------------------------------------------------------------------------!

IF (.NOT. ASSOCIATED(LinkedList%FirstLinkedListNode)) THEN
  WRITE(UNIT_SCREEN,'(2X,A)') "*** No data in LinkedList ***"
  RETURN
END IF

aLinkedListNode => LinkedList%FirstLinkedListNode

WRITE(FormatString,'(A)') '(2(2X,I8))'

iBucket = 0
DO WHILE(ASSOCIATED(aLinkedListNode))
  iBucket = iBucket+1
  BucketID = aLinkedListNode%BucketID
  IF (ALLOCATED(aLinkedListNode%Bucket)) THEN
    SELECT TYPE(Bucket => aLinkedListNode%Bucket)
      TYPE IS(REAL)
        nData = 1
        WRITE(FormatString,'(A,I0,A)') '(2(2X,I8),', nData, '(2X,SP,ES13.6E2))'
      TYPE IS(INTEGER)
        nData = 1
        WRITE(FormatString,'(A,I0,A)') '(2(2X,I8),', nData, '(2X,I8))'
      TYPE IS(tArrayREAL)
        nData = SIZE(Bucket%Data)
        WRITE(FormatString,'(A,I0,A)') '(2(2X,I8),', nData, '(2X,SP,ES13.6E2))'
      TYPE IS(tArrayINTEGER)
        nData = SIZE(Bucket%Data)
        WRITE(FormatString,'(A,I0,A)') '(2(2X,I8),', nData, '(2X,I8))'
    END SELECT
    SELECT TYPE(Bucket => aLinkedListNode%Bucket)
      TYPE IS(REAL)
        WRITE(UNIT_SCREEN,FormatString) iBucket, BucketID, Bucket
      TYPE IS(INTEGER)
        WRITE(UNIT_SCREEN,FormatString) iBucket, BucketID, Bucket
      TYPE IS(tArrayREAL)
        WRITE(UNIT_SCREEN,FormatString) iBucket, BucketID, Bucket%Data
      TYPE IS(tArrayINTEGER)
        WRITE(UNIT_SCREEN,FormatString) iBucket, BucketID, Bucket%Data
    END SELECT
  ELSE
    WRITE(UNIT_SCREEN,FormatString) iBucket, BucketID
  END IF
  aLinkedListNode => aLinkedListNode%NextLinkedListNode
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintLinkedList
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CountLinkedListNodes(LinkedList,nLinkedListNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedList),INTENT(IN) :: LinkedList
INTEGER,INTENT(OUT)          :: nLinkedListNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tLinkedListNode),POINTER :: aLinkedListNode
!----------------------------------------------------------------------------------------------------------------------!

nLinkedListNode = 0
aLinkedListNode => LinkedList%FirstLinkedListNode
DO WHILE(ASSOCIATED(aLinkedListNode))
  nLinkedListNode = nLinkedListNode+1
  aLinkedListNode => aLinkedListNode%NextLinkedListNode
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CountLinkedListNodes
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_LinkedLists
!======================================================================================================================!
