!======================================================================================================================!
!
! (c) Matthew Kennel, Institute for Nonlinear Science (2004)
!
! Licensed under the Academic Free License version 1.1 found below with additional provisions.
!
! The KDTREE2 software is licensed under the terms of the Academic Free
! Software License, listed herein.  In addition, users of this software
! must give appropriate citation in relevant technical documentation or
! journal paper to the author, Matthew B. Kennel, Institute For
! Nonlinear Science, preferably via a reference to the www.arxiv.org
! repository of this document, {\tt www.arxiv.org e-print:
! physics/0408067}.  This requirement will be deemed to be advisory and
! not mandatory as is necessary to permit the free inclusion of the
! present software with any software licensed under the terms of any
! version of the GNU General Public License, or GNU Library General
! Public License.
!
!
! Academic Free License
! Version 1.1
!
! This Academic Free License applies to any original work of authorship
! (the "Original Work") whose owner (the "Licensor") has placed the
! following notice immediately following the copyright notice for the
! Original Work: "Licensed under the Academic Free License version 1.1."
!
! Grant of License. Licensor hereby grants to any person obtaining a
! copy of the Original Work ("You") a world-wide, royalty-free,
! non-exclusive, perpetual, non-sublicenseable license (1) to use, copy,
! modify, merge, publish, perform, distribute and/or sell copies of the
! Original Work and derivative works thereof, and (2) under patent
! claims owned or controlled by the Licensor that are embodied in the
! Original Work as furnished by the Licensor, to make, use, sell and
! offer for sale the Original Work and derivative works thereof, subject
! to the following conditions.
!
! Right of Attribution. Redistributions of the Original Work must
! reproduce all copyright notices in the Original Work as furnished by
! the Licensor, both in the Original Work itself and in any
! documentation and/or other materials provided with the distribution of
! the Original Work in executable form.
!
! Exclusions from License Grant. Neither the names of Licensor, nor the
! names of any contributors to the Original Work, nor any of their
! trademarks or service marks, may be used to endorse or promote
! products derived from this Original Work without express prior written
! permission of the Licensor.
!
! WARRANTY AND DISCLAIMERS. LICENSOR WARRANTS THAT THE COPYRIGHT IN AND
! TO THE ORIGINAL WORK IS OWNED BY THE LICENSOR OR THAT THE ORIGINAL
! WORK IS DISTRIBUTED BY LICENSOR UNDER A VALID CURRENT LICENSE FROM THE
! COPYRIGHT OWNER. EXCEPT AS EXPRESSLY STATED IN THE IMMEDIATELY
! PRECEEDING SENTENCE, THE ORIGINAL WORK IS PROVIDED UNDER THIS LICENSE
! ON AN "AS IS" BASIS, WITHOUT WARRANTY, EITHER EXPRESS OR IMPLIED,
! INCLUDING, WITHOUT LIMITATION, THE WARRANTY OF NON-INFRINGEMENT AND
! WARRANTIES THAT THE ORIGINAL WORK IS MERCHANTABLE OR FIT FOR A
! PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY OF THE ORIGINAL
! WORK IS WITH YOU. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
! PART OF THIS LICENSE. NO LICENSE TO ORIGINAL WORK IS GRANTED HEREUNDER
! EXCEPT UNDER THIS DISCLAIMER.
!
! LIMITATION OF LIABILITY. UNDER NO CIRCUMSTANCES AND UNDER NO LEGAL
! THEORY, WHETHER TORT (INCLUDING NEGLIGENCE), CONTRACT, OR OTHERWISE,
! SHALL THE LICENSOR BE LIABLE TO ANY PERSON FOR ANY DIRECT, INDIRECT,
! SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES OF ANY CHARACTER ARISING
! AS A RESULT OF THIS LICENSE OR THE USE OF THE ORIGINAL WORK INCLUDING,
! WITHOUT LIMITATION, DAMAGES FOR LOSS OF GOODWILL, WORK STOPPAGE,
! COMPUTER FAILURE OR MALFUNCTION, OR ANY AND ALL OTHER COMMERCIAL
! DAMAGES OR LOSSES, EVEN IF SUCH PERSON SHALL HAVE BEEN INFORMED OF THE
! POSSIBILITY OF SUCH DAMAGES. THIS LIMITATION OF LIABILITY SHALL NOT
! APPLY TO LIABILITY FOR DEATH OR PERSONAL INJURY RESULTING FROM SUCH
! PARTY'S NEGLIGENCE TO THE EXTENT APPLICABLE LAW PROHIBITS SUCH
! LIMITATION. SOME JURISDICTIONS DO NOT ALLOW THE EXCLUSION OR
! LIMITATION OF INCIDENTAL OR CONSEQUENTIAL DAMAGES, SO THIS EXCLUSION
! AND LIMITATION MAY NOT APPLY TO YOU.
!
! License to Source Code. The term "Source Code" means the preferred
! form of the Original Work for making modifications to it and all
! available documentation describing how to access and modify the
! Original Work. Licensor hereby agrees to provide a machine-readable
! copy of the Source Code of the Original Work along with each copy of
! the Original Work that Licensor distributes. Licensor reserves the
! right to satisfy this obligation by placing a machine-readable copy of
! the Source Code in an information repository reasonably calculated to
! permit inexpensive and convenient access by You for as long as
! Licensor continues to distribute the Original Work, and by publishing
! the address of that information repository in a notice immediately
! following the copyright notice that applies to the Original Work.
!
! Mutual Termination for Patent Action. This License shall terminate
! automatically and You may no longer exercise any of the rights granted
! to You by this License if You file a lawsuit in any court alleging
! that any OSI Certified open source software that is licensed under any
! license containing this "Mutual Termination for Patent Action" clause
! infringes any patent claims that are essential to use that software.
!
! This license is Copyright (C) 2002 Lawrence E. Rosen. All rights
! reserved. Permission is hereby granted to copy and distribute this
! license without modification. This license may not be modified without
! the express written permission of its copyright owner.
!
!======================================================================================================================!
!
!======================================================================================================================!
#include "main.h"
!======================================================================================================================!
!
!======================================================================================================================!
MODULE MOD_KDTreesBucketPointRegion
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
! GLOBAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,PARAMETER :: BucketSize = 64
!----------------------------------------------------------------------------------------------------------------------!
TYPE tInterval
  REAL :: Lower
  REAL :: Upper
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tKDTreeNode
  INTEGER :: kDim
  INTEGER :: idxIni
  INTEGER :: idxEnd
  REAL    :: kValue
  REAL    :: kValueLeft
  REAL    :: kValueRight
  TYPE(tKDTreeNode),POINTER :: Left
  TYPE(tKDTreeNode),POINTER :: Right
  TYPE(tInterval),POINTER   :: Box(:) => NULL()
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tKDTree
  INTEGER :: nDims
  INTEGER :: nPoints
  LOGICAL :: Sort = .FALSE.
  LOGICAL :: Rearrange = .FALSE.
  REAL,POINTER    :: DataCoords(:,:) => NULL()
  INTEGER,POINTER :: DataIndex(:)    => NULL()
  REAL,POINTER    :: RearrengedDataCoords(:,:) => NULL()
  TYPE(tKDTreeNode),POINTER :: RootNode => NULL()
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tKDTreeNeighbors
  REAL    :: Distance
  INTEGER :: NeighborIndex
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tPriorityQueue
  INTEGER :: HeapSize = 0
  TYPE(tKDTreeNeighbors),POINTER :: Elements(:)
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE tKDTreeSearchRecord
  PRIVATE
  REAL    :: BallSize
  INTEGER :: nDims
  INTEGER :: nNeighbors
  INTEGER :: nNeighborsFound
  INTEGER :: nBestDistances
  INTEGER :: nAlloc
  LOGICAL :: Overflow
  LOGICAL :: Rearrange
  INTEGER :: CenterID
  INTEGER :: CorrelTime
  REAL,POINTER    :: QueryPoint(:)
  REAL,POINTER    :: SquaredDistanceList(:)
  REAL,POINTER    :: DataCoords(:,:)
  INTEGER,POINTER :: DataIndex(:)
  TYPE(tKDTreeNeighbors),POINTER :: SearchResults(:)
  TYPE(tPriorityQueue) :: PriorityQueue
END TYPE
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeSearchRecord),SAVE,TARGET :: SearchRecord
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE CreateKDTreeNode
  MODULE PROCEDURE CreateKDTreeNode
END INTERFACE

INTERFACE DestructKDTreeNode
  MODULE PROCEDURE DestructKDTreeNode
END INTERFACE

INTERFACE ConstructKDTree
  MODULE PROCEDURE ConstructKDTree
END INTERFACE

INTERFACE DestructKDTree
  MODULE PROCEDURE DestructKDTree
END INTERFACE

INTERFACE BuildSubTree
  MODULE PROCEDURE BuildSubTree
END INTERFACE

INTERFACE PrintKDTree
  MODULE PROCEDURE PrintKDTree
END INTERFACE

INTERFACE PrintSubTree
  MODULE PROCEDURE PrintSubTree
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: tKDTree
PUBLIC :: tKDTreeNode
PUBLIC :: tKDTreeNeighbors
PUBLIC :: tKDTreeSearchRecord
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: ConstructKDTree
PUBLIC :: DestructKDTree
PUBLIC :: PrintKDTree
PUBLIC :: FindNearestNeighborsAroundQueryPoint
PUBLIC :: FindNearestNeighborsAroundQueryIndex
PUBLIC :: FindNearestNeighborsInsideBallAroundQueryPoint
PUBLIC :: FindNearestNeighborsInsideBallAroundQueryIndex
PUBLIC :: CountNearestNeighborsInsideBallAroundQueryIndex
PUBLIC :: CountNearestNeighborsInsideBallAroundQueryPoint
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
SUBROUTINE CreateKDTreeNode(KDTreeNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER,INTENT(INOUT) :: KDTreeNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(KDTreeNode)

NULLIFY(KDTreeNode%Left)
NULLIFY(KDTreeNode%Right)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CreateKDTreeNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE DestructKDTreeNode(KDTreeNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER,INTENT(INOUT) :: KDTreeNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (ASSOCIATED(KDTreeNode%Left)) THEN
  CALL DestructKDTreeNode(KDTreeNode%Left)
  NULLIFY(KDTreeNode%Left)
END IF

IF (ASSOCIATED(KDTreeNode%Right)) THEN
  CALL DestructKDTreeNode(KDTreeNode%Right)
  NULLIFY(KDTreeNode%Right)
END IF

IF (ASSOCIATED(KDTreeNode%Box)) THEN
  DEALLOCATE(KDTreeNode%Box)
END IF

DEALLOCATE(KDTreeNode)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DestructKDTreeNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ConstructKDTree(KDTree,DataCoords,Sort,Rearrange)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
REAL,TARGET,INTENT(IN)              :: DataCoords(:,:)
LOGICAL,INTENT(IN),OPTIONAL         :: Sort
LOGICAL,INTENT(IN),OPTIONAL         :: Rearrange
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nDims
INTEGER :: iPoint
INTEGER :: nPoints
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER :: KDTreeParentNode => NULL()
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(KDTree)

nDims   = SIZE(DataCoords,1)
nPoints = SIZE(DataCoords,2)

KDTree%nDims   = nDims
KDTree%nPoints = nPoints

IF (nPoints .GT. 0) THEN
  ALLOCATE(KDTree%DataIndex(1:nPoints))
  DO iPoint=1,nPoints
    KDTree%DataIndex(iPoint) = iPoint
  END DO
  KDTree%DataCoords => DataCoords
END IF

CALL BuildSubTree(KDTree,KDTree%RootNode,KDTreeParentNode,1,nPoints)

IF (PRESENT(Sort)) THEN
  KDTree%Sort = Sort
ELSE
  KDTree%Sort = .FALSE.
END IF

IF (PRESENT(Rearrange)) THEN
  KDTree%Rearrange = Rearrange
ELSE
  KDTree%Rearrange = .TRUE.
END IF

IF (KDTree%Rearrange .EQV. .TRUE.) THEN
  ALLOCATE(KDTree%RearrengedDataCoords(1:KDTree%nDims,1:KDTree%nPoints))
  DO iPoint=1,KDTree%nPoints
    KDTree%RearrengedDataCoords(:,iPoint) = KDTree%DataCoords(:,KDTree%DataIndex(iPoint))
  END DO
ELSE
  NULLIFY(KDTree%RearrengedDataCoords)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ConstructKDTree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE DestructKDTree(KDTree)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL DestructKDTreeNode(KDTree%RootNode)

DEALLOCATE(KDTree%DataIndex)
NULLIFY(KDTree%DataIndex)

IF (KDTree%Rearrange) THEN
  DEALLOCATE(KDTree%RearrengedDataCoords)
  NULLIFY(KDTree%RearrengedDataCoords)
ENDIF

DEALLOCATE(KDTree)
    
!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE DestructKDTree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE BuildSubTree(KDTree,KDTreeNode,KDTreeParentNode,idxIni,idxEnd)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT)     :: KDTree
TYPE(tKDTreeNode),POINTER,INTENT(OUT)   :: KDTreeNode
TYPE(tKDTreeNode),POINTER,INTENT(INOUT) :: KDTreeParentNode
INTEGER,INTENT(IN)                      :: idxIni
INTEGER,INTENT(IN)                      :: idxEnd
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: kDim
INTEGER :: Median
LOGICAL :: Recompute
REAL    :: Average
!----------------------------------------------------------------------------------------------------------------------!

CALL CreateKDTreeNode(KDTreeNode)

ALLOCATE(KDTreeNode%Box(1:KDTree%nDims))

IF (idxEnd .LT. idxIni) THEN
  NULLIFY(KDTreeNode)
  RETURN
END IF

IF ((idxEnd-idxIni) .LE. BucketSize) THEN
  DO ii=1,KDTree%nDims
    CALL SpreadInCoordinates(KDTree,ii,idxIni,idxEnd,KDTreeNode%Box(ii))
  END DO
  KDTreeNode%kDim   = 0
  KDTreeNode%kValue    = 0.0
  KDTreeNode%idxIni = idxIni
  KDTreeNode%idxEnd = idxEnd
  NULLIFY(KDTreeNode%Left)
  NULLIFY(KDTreeNode%Right)
ELSE
  DO ii=1,KDTree%nDims
    Recompute = .TRUE.
    IF (ASSOCIATED(KDTreeParentNode) .EQV. .TRUE.) THEN
      IF (ii .NE. KDTreeParentNode%kDim) THEN
        Recompute = .FALSE.
      END IF
    END IF
    IF (Recompute .EQV. .TRUE.) THEN
      CALL SpreadInCoordinates(KDTree,ii,idxIni,idxEnd,KDTreeNode%Box(ii))
    ELSE
      KDTreeNode%Box(ii) = KDTreeParentNode%Box(ii)
    END IF
  END DO
  
  kDim = MAXLOC(KDTreeNode%Box(1:KDTree%nDims)%Upper-KDTreeNode%Box(1:KDTree%nDims)%Lower,1)
  
  IF (.FALSE.) THEN
    Median = idxIni + (idxEnd-idxIni)/2
    CALL SelectOnCoordinate(KDTree%DataCoords,KDTree%DataIndex,kDim,Median,idxIni,idxEnd)
  ELSE
    IF (.TRUE.) THEN
      Average = SUM(KDTree%DataCoords(kDim,KDTree%DataIndex(idxIni:idxEnd)))/REAL(idxEnd-idxIni+1)
    ELSE
      Average = 0.5*(KDTreeNode%Box(kDim)%Upper+KDTreeNode%Box(kDim)%Lower)
    END IF
    KDTreeNode%kValue = Average
    CALL SelectOnCoordinateByValue(KDTree%DataCoords,KDTree%DataIndex,kDim,Average,idxIni,idxEnd,Median)
  END IF
  
  KDTreeNode%kDim   = kDim
  KDTreeNode%idxIni = idxIni
  KDTreeNode%idxEnd = idxEnd

  CALL BuildSubTree(KDTree,KDTreeNode%Left,KDTreeNode,idxIni,Median)
  CALL BuildSubTree(KDTree,KDTreeNode%Right,KDTreeNode,Median+1,idxEnd)
  
  IF (ASSOCIATED(KDTreeNode%Right) .EQV. .FALSE.) THEN
    KDTreeNode%Box         = KDTreeNode%Left%Box
    KDTreeNode%kValueLeft  = KDTreeNode%Left%Box(kDim)%Upper
    KDTreeNode%kValue      = KDTreeNode%kValueLeft
  ELSEIF (ASSOCIATED(KDTreeNode%Left) .EQV. .FALSE.) THEN
    KDTreeNode%Box         = KDTreeNode%Right%Box
    KDTreeNode%kValueRight = KDTreeNode%Right%Box(kDim)%Lower
    KDTreeNode%kValue      = KDTreeNode%kValueRight
  ELSE
    KDTreeNode%kValueRight = KDTreeNode%Right%Box(kDim)%Lower
    KDTreeNode%kValueLeft  = KDTreeNode%Left%Box(kDim)%Upper
    KDTreeNode%kValue      = 0.5*(KDTreeNode%kValueLeft+KDTreeNode%kValueRight)
    
    KDTreeNode%Box%Upper = MAX(KDTreeNode%Left%Box%Upper,KDTreeNode%Right%Box%Upper)
    KDTreeNode%Box%Lower = MIN(KDTreeNode%Left%Box%Lower,KDTreeNode%Right%Box%Lower)
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildSubTree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SelectOnCoordinate(DataCoords,DataIndex,kDim,Median,idxIni,idxEnd)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(INOUT)    :: DataCoords(:,:)
INTEGER,INTENT(INOUT) :: DataIndex(:)
INTEGER,INTENT(IN)    :: kDim
INTEGER,INTENT(IN)    :: Median
INTEGER,INTENT(IN)    :: idxIni
INTEGER,INTENT(IN)    :: idxEnd
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, l, m, s, t, u
!----------------------------------------------------------------------------------------------------------------------!

l = idxIni
u = idxEnd
DO WHILE (l .LT. u)
    t = DataIndex(l)
    m = l
    DO i =l+1,u
      IF (DataCoords(kDim,DataIndex(i)) .LT. DataCoords(kDim,t)) THEN
          m = m + 1
          s = DataIndex(m)
          DataIndex(m) = DataIndex(i)
          DataIndex(i) = s
      END IF
    END DO
    s = DataIndex(l)
    DataIndex(l) = DataIndex(m)
    DataIndex(m) = s
    IF (m .LE. Median) THEN
      l = m + 1
    END IF
    IF (m .GE. Median) THEN
      u = m - 1
    END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SelectOnCoordinate
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SelectOnCoordinateByValue(DataCoords,DataIndex,kDim,Alpha,idxIni,idxEnd,Median)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(INOUT)    :: DataCoords(:,:)
INTEGER,INTENT(INOUT) :: DataIndex(:)
INTEGER,INTENT(IN)    :: kDim
REAL,INTENT(IN)       :: Alpha
INTEGER,INTENT(IN)    :: idxIni
INTEGER,INTENT(IN)    :: idxEnd
INTEGER,INTENT(OUT)   :: Median
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: l, s, u
!----------------------------------------------------------------------------------------------------------------------!

l = idxIni
u = idxEnd
DO WHILE (l .LT. u)
  IF (DataCoords(kDim,DataIndex(l)) .LE. Alpha) THEN
    l = l+1
  ELSE
    s = DataIndex(l)
    DataIndex(l) = DataIndex(u)
    DataIndex(u) = s
    u = u-1
  END IF
END DO

IF (DataCoords(kDim,DataIndex(l)) .LE. Alpha) THEN
  Median = l
ELSE
  Median = l-1
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SelectOnCoordinateByValue
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SpreadInCoordinates(KDTree,kDim,idxIni,idxEnd,Interval)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
INTEGER,INTENT(IN)                  :: kDim
INTEGER,INTENT(IN)                  :: idxIni
INTEGER,INTENT(IN)                  :: idxEnd
TYPE(tInterval),INTENT(OUT)         :: Interval
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
REAL,POINTER    :: DataCoords(:,:)
INTEGER,POINTER :: DataIndex(:)
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii, ulocal
REAL    :: last, lmax, lmin, t, smin, smax
!----------------------------------------------------------------------------------------------------------------------!

DataCoords => KDTree%DataCoords
DataIndex  => KDTree%DataIndex

smin = DataCoords(kDim,DataIndex(idxIni))
smax = smin

ulocal = idxEnd

DO ii=idxIni+2,ulocal,2
  lmin = DataCoords(kDim,DataIndex(ii-1))
  lmax = DataCoords(kDim,DataIndex(ii))
  IF (lmin .GT. lmax) THEN
    t = lmin
    lmin = lmax
    lmax = t
  END IF
  IF (smin .GT. lmin) THEN
    smin = lmin
  END IF
  IF (smax .LT. lmax) THEN
    smax = lmax
  END IF
END DO

IF (ii .EQ. ulocal+1) THEN
  last = DataCoords(kDim,DataIndex(ulocal))
  IF (smin .GT. last) THEN
    smin = last
  END IF
  IF (smax .LT. last) THEN
    smax = last
  END IF
END IF

Interval%Lower = smin
Interval%Upper = smax

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SpreadInCoordinates
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PrintKDTree(KDTree)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nDims
INTEGER :: iPoint
INTEGER :: nPoints
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: FormatString
!----------------------------------------------------------------------------------------------------------------------!

nDims   = KDTree%nDims
nPoints = KDTree%nPoints

SELECT CASE(nDims)
  CASE(2)
    FormatString = "(2X,I4,2X,I4,2(2X,SP,ES13.6))"
  CASE(3)
    FormatString = "(2X,I4,2X,I4,3(2X,SP,ES13.6))"
END SELECT

DO iPoint=1,nPoints
  WRITE(UNIT_SCREEN,FormatString) iPoint, KDTree%DataIndex(iPoint), KDTree%DataCoords(1:nDims,iPoint)
END DO
WRITE(*,*)

CALL PrintSubTree(KDTree,KDTree%RootNode)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintKDTree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE PrintSubTree(KDTree,KDTreeNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(IN)     :: KDTree
TYPE(tKDTreeNode),POINTER,INTENT(IN) :: KDTreeNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

WRITE(*,"(A,1X,I0,1X,A,1X,SP,ES13.6)") "kDim:", KDTreeNode%kDim, "kValue:", KDTreeNode%kValue
WRITE(*,"(A,1X,I0,1X,A,1X,I0)") "idxIni:", KDTreeNode%idxIni, "idxEnd:", KDTreeNode%idxEnd
WRITE(*,*)

IF (ASSOCIATED(KDTreeNode%Left) .EQV. .TRUE.) THEN
  WRITE(*,"(A)") "KDTreeNode-Left"
  CALL PrintSubTree(KDTree,KDTreeNode%Left)
END IF

IF (ASSOCIATED(KDTreeNode%Right) .EQV. .TRUE.) THEN
  WRITE(*,"(A)") "KDTreeNode-Right"
  CALL PrintSubTree(KDTree,KDTreeNode%Right)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PrintSubTree
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PriorityQueueCreate(NeighborsList,PriorityQueue)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors),TARGET,INTENT(IN) :: NeighborsList(:)
TYPE(tPriorityQueue),INTENT(OUT)         :: PriorityQueue
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: nAlloc
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

nAlloc = SIZE(NeighborsList,1)
IF (nAlloc .LT. 1) THEN
  ErrorMessage = "PriorityQueueCreate: Input arrays must be allocated"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

PriorityQueue%Elements => NeighborsList
PriorityQueue%HeapSize = 0

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PriorityQueueCreate
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE Heapify(PriorityQueue,ii)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(INOUT) :: PriorityQueue
INTEGER,INTENT(IN)                         :: ii
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors) :: NeighborsList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, l, r, largest
REAL    :: pri_i, pri_l, pri_r, pri_largest
!----------------------------------------------------------------------------------------------------------------------!

i = ii
DO
  l = 2*i
  r = l+1

  IF (l .GT. PriorityQueue%HeapSize) THEN
    EXIT 
  ELSE
    pri_i = PriorityQueue%Elements(I)%Distance
    pri_l = PriorityQueue%Elements(L)%Distance 
    IF (pri_l .GT. pri_i) THEN
      largest = l
      pri_largest = pri_l
    ELSE
      largest = i
      pri_largest = pri_i
    ENDIF

    IF (r .LE. PriorityQueue%HeapSize) THEN
        pri_r = PriorityQueue%Elements(R)%Distance
        IF (pri_r .GT. pri_largest) THEN
          largest = r
        END IF
    END IF
  END IF

  IF (largest .NE. i) THEN
    NeighborsList = PriorityQueue%Elements(i)
    PriorityQueue%Elements(i) = PriorityQueue%Elements(largest)
    PriorityQueue%Elements(largest) = NeighborsList

    i = largest
    CYCLE
  ELSE
    RETURN
  END IF
END DO 

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Heapify
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PriorityQueueMaximum(PriorityQueue,NeighborsList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(IN) :: PriorityQueue
TYPE(tKDTreeNeighbors),INTENT(OUT)      :: NeighborsList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (PriorityQueue%HeapSize .GT. 0) THEN
  NeighborsList = PriorityQueue%Elements(1)
ELSE
  ErrorMessage = "PriorityQueueMaximum: HeapSize < 1"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PriorityQueueMaximum
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION PriorityQueueMaximumPriority(PriorityQueue) RESULT(Distance)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(IN) :: PriorityQueue
REAL                                    :: Distance
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (PriorityQueue%HeapSize .GT. 0) THEN
  Distance = PriorityQueue%Elements(1)%Distance
ELSE
  ErrorMessage = "PriorityQueueMaximum: HeapSize < 1"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION PriorityQueueMaximumPriority
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PriorityQueueExtractMaximum(PriorityQueue,NeighborsList)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(INOUT) :: PriorityQueue
TYPE(tKDTreeNeighbors),INTENT(OUT)         :: NeighborsList
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (PriorityQueue%HeapSize .GE. 1) THEN
  NeighborsList = PriorityQueue%Elements(1)
  PriorityQueue%Elements(1) = PriorityQueue%Elements(PriorityQueue%HeapSize)
  PriorityQueue%HeapSize    = PriorityQueue%HeapSize-1
  CALL Heapify(PriorityQueue,1)
ELSE
  ErrorMessage = "PriorityQueueExtractMaximum: Attempted to pop non-positive PriorityQueue"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PriorityQueueExtractMaximum
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION PriorityQueueInsert(PriorityQueue,Distance,NeighborIndex) RESULT(InsertedDistance)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(IN) :: PriorityQueue
REAL,INTENT(IN)                         :: Distance
INTEGER,INTENT(IN)                      :: NeighborIndex
REAL                                    :: InsertedDistance
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii, IsParent
REAL    :: ParentDistance
!----------------------------------------------------------------------------------------------------------------------!

PriorityQueue%HeapSize = PriorityQueue%HeapSize+1

ii = PriorityQueue%HeapSize
DO WHILE (ii .GT. 1)
  IsParent       = INT(ii/2)
  ParentDistance = PriorityQueue%Elements(IsParent)%Distance
  IF (Distance .GT. ParentDistance) THEN
    PriorityQueue%Elements(ii)%Distance      = ParentDistance
    PriorityQueue%Elements(ii)%NeighborIndex = PriorityQueue%Elements(IsParent)%NeighborIndex
    ii = IsParent
  ELSE
    EXIT
  END IF
END DO

PriorityQueue%Elements(ii)%Distance      = Distance
PriorityQueue%Elements(ii)%NeighborIndex = NeighborIndex

InsertedDistance = PriorityQueue%Elements(1)%Distance

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION PriorityQueueInsert
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PriorityQueueAdjustHeap(PriorityQueue,ii)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(INOUT) :: PriorityQueue
INTEGER,INTENT(IN)                         :: ii
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors) :: NeighborsList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: Parent, Child, N
REAL    :: PriorityChildDistance
!----------------------------------------------------------------------------------------------------------------------!

NeighborsList = PriorityQueue%Elements(ii)

Parent = ii
Child  = 2*ii
N      = PriorityQueue%HeapSize

DO WHILE (Child .LE. 1)
  IF (Child .LT. N) THEN
    IF (PriorityQueue%Elements(Child)%Distance .LT. PriorityQueue%Elements(Child+1)%Distance) THEN
      Child = Child+1
    END IF
  END IF
  PriorityChildDistance = PriorityQueue%Elements(Child)%Distance
  IF (NeighborsList%Distance .GE. PriorityChildDistance) THEN
    EXIT
  ELSE
    PriorityQueue%Elements(Parent) = PriorityQueue%Elements(Child)
    Parent = Child
    Child  = 2*Parent
  END IF
END DO

PriorityQueue%Elements(Parent) = NeighborsList

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PriorityQueueAdjustHeap
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION PriorityQueueReplaceMaximum(PriorityQueue,Distance,NeighborIndex) RESULT(ReplacedDistance)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(INOUT) :: PriorityQueue
REAL,INTENT(IN)                            :: Distance
INTEGER,INTENT(IN)                         :: NeighborIndex
REAL                                       :: ReplacedDistance
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors) :: NeighborsList
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: Parent, Child, N
REAL    :: PriorityChildDistance, PriorityChildDistance1
!----------------------------------------------------------------------------------------------------------------------!

IF (.TRUE.) THEN
  N = PriorityQueue%HeapSize
  IF (N .GE. 1) THEN
    Parent = 1
    Child  = 2
    loop: DO WHILE (Child .LE. N)
      PriorityChildDistance = PriorityQueue%Elements(Child)%Distance
      IF (Child .LT. N) THEN
        PriorityChildDistance1 = PriorityQueue%Elements(Child+1)%Distance
        IF (PriorityChildDistance .LT. PriorityChildDistance1) THEN
          Child                 = Child+1
          PriorityChildDistance = PriorityChildDistance1
        END IF
      END IF
      
      IF (Distance .GE. PriorityChildDistance) THEN
        EXIT loop
      ELSE
        PriorityQueue%Elements(Parent) = PriorityQueue%Elements(Child)
        Parent = Child
        Child  = 2*Parent
      END IF
    END DO loop
    PriorityQueue%Elements(Parent)%Distance      = Distance
    PriorityQueue%Elements(Parent)%NeighborIndex = NeighborIndex
    ReplacedDistance                             = PriorityQueue%Elements(1)%Distance
  ELSE
    PriorityQueue%Elements(1)%Distance      = Distance
    PriorityQueue%Elements(1)%NeighborIndex = NeighborIndex
    ReplacedDistance                        = Distance
  END IF
ELSE
  CALL PriorityQueueExtractMaximum(PriorityQueue,NeighborsList)
  NeighborsList%Distance      = Distance
  NeighborsList%NeighborIndex = NeighborIndex
  ReplacedDistance            = PriorityQueueInsert(PriorityQueue,Distance,NeighborIndex)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION PriorityQueueReplaceMaximum
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PriorityQueueDelete(PriorityQueue,ii)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER,INTENT(INOUT) :: PriorityQueue
INTEGER,INTENT(IN)                         :: ii
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF ((ii .LT. 1) .OR. (ii .GT. PriorityQueue%HeapSize)) THEN
  ErrorMessage = "PriorityQueueExtractMaximum: Attempted to remove out of bounds element"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

PriorityQueue%Elements(ii) = PriorityQueue%Elements(PriorityQueue%HeapSize)
PriorityQueue%HeapSize     = PriorityQueue%HeapSize-1

CALL Heapify(PriorityQueue,ii)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PriorityQueueDelete
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindNearestNeighborsAroundQueryPoint(KDTree,QueryPoint,nNeighbors,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
REAL,TARGET,INTENT(IN)              :: QueryPoint(:)
INTEGER,INTENT(IN)                  :: nNeighbors
TYPE(tKDTreeNeighbors),TARGET       :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

SearchRecord%BallSize        = HUGE(1.0)
SearchRecord%nNeighbors      = nNeighbors
SearchRecord%nNeighborsFound = 0
SearchRecord%CenterID        = -1
SearchRecord%CorrelTime      = 0
SearchRecord%Overflow        = .FALSE.
SearchRecord%nDims           = KDTree%nDims
SearchRecord%nAlloc          = nNeighbors
SearchRecord%Rearrange       = KDTree%Rearrange
SearchRecord%QueryPoint      => QueryPoint
SearchRecord%DataIndex       => KDTree%DataIndex
SearchRecord%SearchResults   => SearchResults

IF (SearchRecord%Rearrange .EQV. .TRUE.) THEN
  SearchRecord%DataCoords => KDTree%RearrengedDataCoords
ELSE
  SearchRecord%DataCoords => KDTree%DataCoords
END IF

CALL ValidateQueryStorage(nNeighbors)
CALL PriorityQueueCreate(SearchResults,SearchRecord%PriorityQueue)
CALL Search(KDTree%RootNode)

IF (KDTree%Sort .EQV. .TRUE.) THEN
  CALL SortResults(nNeighbors,SearchResults)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindNearestNeighborsAroundQueryPoint
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindNearestNeighborsAroundQueryIndex(KDTree,PointIndex,CorrelTime,nNeighbors,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
INTEGER,INTENT(IN)                  :: PointIndex
INTEGER,INTENT(IN)                  :: CorrelTime
INTEGER,INTENT(IN)                  :: nNeighbors
TYPE(tKDTreeNeighbors),TARGET       :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(SearchRecord%QueryPoint(1:KDTree%nDims))

SearchRecord%BallSize        = HUGE(1.0)
SearchRecord%nNeighbors      = nNeighbors
SearchRecord%nNeighborsFound = 0
SearchRecord%CenterID        = PointIndex
SearchRecord%CorrelTime      = CorrelTime
SearchRecord%Overflow        = .FALSE.
SearchRecord%nDims           = KDTree%nDims
SearchRecord%nAlloc          = nNeighbors
SearchRecord%Rearrange       = KDTree%Rearrange
SearchRecord%QueryPoint      = KDTree%DataCoords(:,PointIndex)
SearchRecord%DataIndex       => KDTree%DataIndex
SearchRecord%SearchResults   => SearchResults

IF (SearchRecord%Rearrange .EQV. .TRUE.) THEN
  SearchRecord%DataCoords => KDTree%RearrengedDataCoords
ELSE
  SearchRecord%DataCoords => KDTree%DataCoords
END IF

CALL ValidateQueryStorage(nNeighbors)
CALL PriorityQueueCreate(SearchResults,SearchRecord%PriorityQueue)
CALL Search(KDTree%RootNode)

IF (KDTree%Sort .EQV. .TRUE.) THEN
  CALL SortResults(nNeighbors,SearchResults)
END IF

DEALLOCATE(SearchRecord%QueryPoint)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindNearestNeighborsAroundQueryIndex
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindNearestNeighborsInsideBallAroundQueryPoint(KDTree,QueryPoint,Radius2,nAlloc,nNeighborsFound,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
REAL,TARGET,INTENT(IN)              :: QueryPoint(:)
REAL,INTENT(IN)                     :: Radius2
INTEGER,INTENT(IN)                  :: nAlloc
INTEGER,INTENT(OUT)                 :: nNeighborsFound
TYPE(tKDTreeNeighbors),TARGET       :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

SearchRecord%BallSize        = Radius2
SearchRecord%nNeighbors      = 0
SearchRecord%nNeighborsFound = 0
SearchRecord%CenterID        = -1
SearchRecord%CorrelTime      = 0
SearchRecord%QueryPoint      => QueryPoint
SearchRecord%SearchResults   => SearchResults

CALL ValidateQueryStorage(nAlloc)

SearchRecord%nAlloc          = nAlloc
SearchRecord%Overflow        = .FALSE.
SearchRecord%nDims           = KDTree%nDims
SearchRecord%Rearrange       = KDTree%Rearrange
SearchRecord%DataIndex       => KDTree%DataIndex

IF (SearchRecord%Rearrange .EQV. .TRUE.) THEN
  SearchRecord%DataCoords => KDTree%RearrengedDataCoords
ELSE
  SearchRecord%DataCoords => KDTree%DataCoords
END IF

CALL Search(KDTree%RootNode)

nNeighborsFound = SearchRecord%nNeighborsFound

IF (KDTree%Sort .EQV. .TRUE.) THEN
  CALL SortResults(nNeighborsFound,SearchResults)
END IF

IF (SearchRecord%Overflow .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*) "FindNearestNeighborsInsideBall: nNeighborsFound > nAlloc"
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindNearestNeighborsInsideBallAroundQueryPoint
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindNearestNeighborsInsideBallAroundQueryIndex(KDTree,PointIndex,CorrelTime,Radius2,nAlloc,nNeighborsFound,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
INTEGER,INTENT(IN)                  :: PointIndex
INTEGER,INTENT(IN)                  :: CorrelTime
REAL,INTENT(IN)                     :: Radius2
INTEGER,INTENT(IN)                  :: nAlloc
INTEGER,INTENT(OUT)                 :: nNeighborsFound
TYPE(tKDTreeNeighbors),TARGET       :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(SearchRecord%QueryPoint(1:KDTree%nDims))

SearchRecord%QueryPoint      = KDTree%DataCoords(:,PointIndex)

SearchRecord%BallSize        = Radius2
SearchRecord%nNeighbors      = 0
SearchRecord%nNeighborsFound = 0
SearchRecord%CenterID        = PointIndex
SearchRecord%CorrelTime      = CorrelTime
SearchRecord%nAlloc          = nAlloc
SearchRecord%Overflow        = .FALSE.
SearchRecord%nDims           = KDTree%nDims
SearchRecord%Rearrange       = KDTree%Rearrange
SearchRecord%SearchResults   => SearchResults

CALL ValidateQueryStorage(nAlloc)

SearchRecord%DataIndex       => KDTree%DataIndex

IF (SearchRecord%Rearrange .EQV. .TRUE.) THEN
  SearchRecord%DataCoords => KDTree%RearrengedDataCoords
ELSE
  SearchRecord%DataCoords => KDTree%DataCoords
END IF

CALL Search(KDTree%RootNode)

nNeighborsFound = SearchRecord%nNeighborsFound

IF (KDTree%Sort .EQV. .TRUE.) THEN
  CALL SortResults(nNeighborsFound,SearchResults)
END IF

IF (SearchRecord%Overflow .EQV. .TRUE.) THEN
  WRITE(UNIT_SCREEN,*) "FindNearestNeighborsInsideBall: nNeighborsFound > nAlloc"
END IF

DEALLOCATE(SearchRecord%QueryPoint)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindNearestNeighborsInsideBallAroundQueryIndex
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CountNearestNeighborsInsideBallAroundQueryPoint(KDTree,QueryPoint,Radius2) RESULT(nNeighborsFound)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
REAL,TARGET,INTENT(IN)              :: QueryPoint(:)
REAL,INTENT(IN)                     :: Radius2
INTEGER                             :: nNeighborsFound
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

SearchRecord%QueryPoint      => QueryPoint
SearchRecord%BallSize        = Radius2
SearchRecord%nNeighbors      = 0
SearchRecord%nNeighborsFound = 0
SearchRecord%CenterID        = -1
SearchRecord%CorrelTime      = 0
SearchRecord%nDims           = KDTree%nDims
SearchRecord%nAlloc          = 0
SearchRecord%Overflow        = .FALSE.
SearchRecord%DataIndex       => KDTree%DataIndex
SearchRecord%Rearrange       = KDTree%Rearrange

IF (SearchRecord%Rearrange .EQV. .TRUE.) THEN
  SearchRecord%DataCoords => KDTree%RearrengedDataCoords
ELSE
  SearchRecord%DataCoords => KDTree%DataCoords
END IF

NULLIFY(SearchRecord%SearchResults)

CALL Search(KDTree%RootNode)

nNeighborsFound = SearchRecord%nNeighborsFound

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CountNearestNeighborsInsideBallAroundQueryPoint
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION CountNearestNeighborsInsideBallAroundQueryIndex(KDTree,PointIndex,CorrelTime,Radius2) RESULT(nNeighborsFound)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT) :: KDTree
INTEGER,INTENT(IN)                  :: PointIndex
INTEGER,INTENT(IN)                  :: CorrelTime
REAL,INTENT(IN)                     :: Radius2
INTEGER                             :: nNeighborsFound
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(SearchRecord%QueryPoint(1:KDTree%nDims))

SearchRecord%QueryPoint      = KDTree%DataCoords(:,PointIndex)

SearchRecord%BallSize        = Radius2
SearchRecord%nNeighbors      = 0
SearchRecord%nNeighborsFound = 0
SearchRecord%CenterID        = PointIndex
SearchRecord%CorrelTime      = CorrelTime
SearchRecord%nDims           = KDTree%nDims
SearchRecord%nAlloc          = 0
SearchRecord%Overflow        = .FALSE.
SearchRecord%DataIndex       => KDTree%DataIndex
SearchRecord%Rearrange       = KDTree%Rearrange

IF (SearchRecord%Rearrange .EQV. .TRUE.) THEN
  SearchRecord%DataCoords => KDTree%RearrengedDataCoords
ELSE
  SearchRecord%DataCoords => KDTree%DataCoords
END IF

NULLIFY(SearchRecord%SearchResults)

CALL Search(KDTree%RootNode)

nNeighborsFound = SearchRecord%nNeighborsFound


!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION CountNearestNeighborsInsideBallAroundQueryIndex
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ValidateQueryStorage(nAlloc)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nAlloc
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(SearchRecord%SearchResults,1) .LT. nAlloc) THEN
  ErrorMessage = "ValidateQueryStorage: Not enough storage has been provided for SearchResults(1:nAlloc)"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ValidateQueryStorage
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION SquaredDistance(nDims,Point1,Point2) RESULT(Distance2)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nDims
REAL,INTENT(IN)    :: Point1(:)
REAL,INTENT(IN)    :: Point2(:)
REAL               :: Distance2
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

Distance2 = SUM((Point1(1:nDims)-Point2(1:nDims))**2)

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION SquaredDistance
!======================================================================================================================!
!
!
!
!======================================================================================================================!
RECURSIVE SUBROUTINE Search(KDTreeNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER,INTENT(INOUT) :: KDTreeNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: kDim
REAL    :: kValue
REAL    :: BallSize
REAL    :: Distance2
!----------------------------------------------------------------------------------------------------------------------!
REAL,POINTER            :: QueryPoint(:)
TYPE(tInterval),POINTER :: Box(:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER :: KDTreeNodeCloser
TYPE(tKDTreeNode),POINTER :: KDTreeNodeFarther
!----------------------------------------------------------------------------------------------------------------------!

IF ((ASSOCIATED(KDTreeNode%Left) .AND. ASSOCIATED(KDTreeNode%Right)) .EQV. .FALSE.) THEN
  IF (SearchRecord%nNeighbors .EQ. 0) THEN
    CALL ProcessTerminalNodeFixedBall(KDTreeNode)
  ELSE
    CALL ProcessTerminalNode(KDTreeNode)
  END IF
ELSE
  QueryPoint => SearchRecord%QueryPoint
  kDim       = KDTreeNode%kDim
  kValue     = QueryPoint(kDim)
  IF (kValue .LT. KDTreeNode%kValue) THEN
    KDTreeNodeCloser  => KDTreeNode%Left
    KDTreeNodeFarther => KDTreeNode%Right
    Distance2         = (KDTreeNode%kValueRight-kValue)**2
  ELSE
    KDTreeNodeCloser  => KDTreeNode%Right
    KDTreeNodeFarther => KDTreeNode%Left
    Distance2         = (KDTreeNode%kValueLeft-kValue)**2
  END IF

  IF (ASSOCIATED(KDTreeNodeCloser) .EQV. .TRUE.) THEN
    CALL Search(KDTreeNodeCloser)
  END IF

  IF (ASSOCIATED(KDTreeNodeFarther) .EQV. .TRUE.) THEN
    BallSize = SearchRecord%BallSize
    IF (Distance2 .LE. BallSize) THEN
      Box => KDTreeNode%Box
      DO ii=1,SearchRecord%nDims
        IF (ii .NE. kDim) THEN
          Distance2 = Distance2 + DistanceFromBound(QueryPoint(ii),Box(ii)%Lower,Box(ii)%Upper)
          IF (Distance2 .GT. BallSize) THEN
            RETURN
          END IF
        END IF
      END DO
      CALL Search(KDTreeNodeFarther)
    END IF
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE Search
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION DistanceFromBound(x,aMin,aMax) RESULT(Distance2)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)    :: x
REAL,INTENT(IN)    :: aMin
REAL,INTENT(IN)    :: aMax
REAL               :: Distance2
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (x .GT. aMax) THEN
  Distance2 = (x-aMax)**2
ELSE
  IF (x .LT. aMin) THEN
    Distance2 = (aMin-x)**2
  ELSE
    Distance2 = 0.0
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION DistanceFromBound
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION BoxInSearchRange(KDTreeNode,SearchRecord) RESULT(Flag)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER,INTENT(IN)         :: KDTreeNode
TYPE(tKDTreeSearchRecord),POINTER,INTENT(IN) :: SearchRecord
LOGICAL                                      :: Flag
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: nDims
REAL    :: BallSize
REAL    :: Distance2
REAL    :: Lower
REAL    :: Upper
!----------------------------------------------------------------------------------------------------------------------!

nDims     = SearchRecord%nDims
BallSize  = SearchRecord%BallSize
Distance2 = 0.0

Flag = .TRUE.
DO ii=1,nDims
  Lower = KDTreeNode%Box(ii)%Lower
  Upper = KDTreeNode%Box(ii)%Upper
  Distance2 = Distance2 + DistanceFromBound(SearchRecord%QueryPoint(ii),Lower,Upper)
  IF (Distance2 .GT. BallSize) THEN
    Flag = .FALSE.
    RETURN
  END IF
END DO
Flag = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION BoxInSearchRange
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ProcessTerminalNode(KDTreeNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER,INTENT(INOUT) :: KDTreeNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: kk
INTEGER :: nDims
INTEGER :: aIndex
INTEGER :: CenterID
INTEGER :: CorrelTime
LOGICAL :: Rearrange
REAL    :: BallSize
REAL    :: Distance2
REAL    :: NewPriority
!----------------------------------------------------------------------------------------------------------------------!
REAL,POINTER    :: QueryPoint(:)
INTEGER,POINTER :: DataIndex(:)
REAL,POINTER    :: DataCoords(:,:)
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tPriorityQueue),POINTER :: PriorityQueue
!----------------------------------------------------------------------------------------------------------------------!

nDims         = SearchRecord%nDims
BallSize      = SearchRecord%BallSize
Rearrange     = SearchRecord%Rearrange
CenterID      = SearchRecord%CenterID
CorrelTime    = SearchRecord%CorrelTime
QueryPoint    => SearchRecord%QueryPoint
PriorityQueue => SearchRecord%PriorityQueue
DataIndex     => SearchRecord%DataIndex
DataCoords    => SearchRecord%DataCoords

MainLoop: DO ii=KDTreeNode%idxIni,KDTreeNode%idxEnd
  IF (Rearrange .EQV. .TRUE.) THEN
    Distance2 = 0.0
    DO kk=1,nDims
      Distance2 = Distance2 + (DataCoords(kk,ii)-QueryPoint(kk))**2
      IF (Distance2 .GT. BallSize) THEN
        CYCLE MainLoop
      END IF
    END DO
    aIndex = DataIndex(ii)
  ELSE
    aIndex = DataIndex(ii)
    Distance2 = 0.0
    DO kk=1,nDims
      Distance2 = Distance2 + (DataCoords(kk,aIndex)-QueryPoint(kk))**2
      IF (Distance2 .GT. BallSize) THEN
        CYCLE MainLoop
      END IF
    END DO
  END IF
 
  IF (CenterID .GT. 0) THEN
    IF (ABS(aIndex-CenterID) .LT. CorrelTime) THEN
      CYCLE MainLoop
    END IF
  END IF
  
  IF (SearchRecord%nNeighborsFound .LT. SearchRecord%nNeighbors) THEN
    SearchRecord%nNeighborsFound = SearchRecord%nNeighborsFound+1
    NewPriority = PriorityQueueInsert(PriorityQueue,Distance2,aIndex)
    IF (SearchRecord%nNeighborsFound .EQ. SearchRecord%nNeighbors) THEN
      BallSize = NewPriority
    END IF
  ELSE
    BallSize = PriorityQueueReplaceMaximum(PriorityQueue,Distance2,aIndex)
  END IF
END DO MainLoop

SearchRecord%BallSize = BallSize

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ProcessTerminalNode
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ProcessTerminalNodeFixedBall(KDTreeNode)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNode),POINTER,INTENT(INOUT) :: KDTreeNode
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: kk
INTEGER :: nDims
INTEGER :: aIndex
INTEGER :: CenterID
INTEGER :: CorrelTime
INTEGER :: nNeighbors
INTEGER :: nNeighborsFound
LOGICAL :: Rearrange
REAL    :: BallSize
REAL    :: Distance2
!----------------------------------------------------------------------------------------------------------------------!
REAL,POINTER    :: QueryPoint(:)
INTEGER,POINTER :: DataIndex(:)
REAL,POINTER    :: DataCoords(:,:)
!----------------------------------------------------------------------------------------------------------------------!

nDims           = SearchRecord%nDims
BallSize        = SearchRecord%BallSize
Rearrange       = SearchRecord%Rearrange
CenterID        = SearchRecord%CenterID
CorrelTime      = SearchRecord%CorrelTime
nNeighbors      = SearchRecord%nNeighbors
nNeighborsFound = SearchRecord%nNeighborsFound
QueryPoint      => SearchRecord%QueryPoint
DataIndex       => SearchRecord%DataIndex
DataCoords      => SearchRecord%DataCoords

MainLoop: DO ii=KDTreeNode%idxIni,KDTreeNode%idxEnd
  IF (Rearrange .EQV. .TRUE.) THEN
    Distance2 = 0.0
    DO kk=1,nDims
      Distance2 = Distance2 + (DataCoords(kk,ii)-QueryPoint(kk))**2
      IF (Distance2 .GT. BallSize) THEN
        CYCLE MainLoop
      END IF
    END DO
    aIndex = DataIndex(ii)
  ELSE
    aIndex = DataIndex(ii)
    Distance2 = 0.0
    DO kk=1,nDims
      Distance2 = Distance2 + (DataCoords(kk,aIndex)-QueryPoint(kk))**2
      IF (Distance2 .GT. BallSize) THEN
        CYCLE MainLoop
      END IF
    END DO
  END IF
 
  IF (CenterID .GT. 0) THEN
    IF (ABS(aIndex-CenterID) .LT. CorrelTime) THEN
      CYCLE MainLoop
    END IF
  END IF
  
  nNeighborsFound = nNeighborsFound+1
  IF (nNeighborsFound .GT. SearchRecord%nAlloc) THEN
    SearchRecord%Overflow = .TRUE.
  ELSE
    SearchRecord%SearchResults(nNeighborsFound)%Distance      = Distance2
    SearchRecord%SearchResults(nNeighborsFound)%NeighborIndex = aIndex
  END IF
END DO MainLoop

SearchRecord%nNeighborsFound = nNeighborsFound

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ProcessTerminalNodeFixedBall
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindNearestNeighborsBruteForce(KDTree,QueryPoint,nNeighbors,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT)  :: KDTree
REAL,INTENT(IN)                      :: QueryPoint(:)
INTEGER,INTENT(IN)                   :: nNeighbors
TYPE(tKDTreeNeighbors),INTENT(INOUT) :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: jj
INTEGER :: kk
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE :: AllDistances(:)
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(AllDistances(1:KDTree%nPoints))

DO ii=1,KDTree%nPoints
  AllDistances(ii) = SquaredDistance(KDTree%nDims,QueryPoint,KDTree%DataCoords(:,ii))
END DO

DO ii=1,nNeighbors
  SearchResults(ii)%Distance      = HUGE(1.0)
  SearchResults(ii)%NeighborIndex = -1
END DO

DO ii=1,KDTree%nPoints
  IF (AllDistances(ii) .LT. SearchResults(nNeighbors)%Distance) THEN
    DO jj=1,nNeighbors
      IF (AllDistances(ii) .LT. SearchResults(jj)%Distance) THEN
        EXIT
      END IF
    END DO
    DO kk=nNeighbors-1,jj,-1
      SearchResults(kk+1) = SearchResults(kk)
    END DO
    SearchResults(jj)%Distance      = AllDistances(ii)
    SearchResults(jj)%NeighborIndex = ii
  END IF
END DO

DEALLOCATE(AllDistances)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindNearestNeighborsBruteForce
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE FindNearestNeighborsInsideBallBruteForce(KDTree,QueryPoint,Radius2,nNeighborsFound,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTree),POINTER,INTENT(INOUT)  :: KDTree
REAL,INTENT(IN)                      :: QueryPoint(:)
REAL,INTENT(IN)                      :: Radius2
INTEGER,INTENT(OUT)                  :: nNeighborsFound
TYPE(tKDTreeNeighbors),INTENT(INOUT) :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: nAlloc
!----------------------------------------------------------------------------------------------------------------------!
REAL,ALLOCATABLE :: AllDistances(:)
!----------------------------------------------------------------------------------------------------------------------!

ALLOCATE(AllDistances(1:KDTree%nPoints))

DO ii=1,KDTree%nPoints
  AllDistances(ii) = SquaredDistance(KDTree%nDims,QueryPoint,KDTree%DataCoords(:,ii))
END DO

nNeighborsFound = 0
nAlloc          = SIZE(SearchResults,1)

DO ii=1,KDTree%nPoints
  IF (AllDistances(ii) .LT. Radius2) THEN
    IF (nNeighborsFound .LT. nAlloc) THEN
      nNeighborsFound = nNeighborsFound+1
      SearchResults(nNeighborsFound)%Distance      = AllDistances(ii)
      SearchResults(nNeighborsFound)%NeighborIndex = ii
    END IF
  END IF
END DO

DEALLOCATE(AllDistances)

CALL SortResults(nNeighborsFound,SearchResults)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE FindNearestNeighborsInsideBallBruteForce
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE SortResults(nNeighborsFound,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)            :: nNeighborsFound 
TYPE(tKDTreeNeighbors),TARGET :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

IF (nNeighborsFound .GT. 1) THEN
  CALL HeapSortStruct(nNeighborsFound,SearchResults)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE SortResults
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE HeapSort(N,DataArray,DataIndex)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)    :: N
INTEGER,INTENT(INOUT) :: DataIndex(:)
REAL,INTENT(INOUT)    :: DataArray(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: jj
INTEGER :: iLeft
INTEGER :: iRight
INTEGER :: iValue
REAL    :: aValue
!----------------------------------------------------------------------------------------------------------------------!

iLeft  = N/2+1
iRight = N

IF (N .EQ. 1) THEN
  RETURN
END IF

DO
  IF (iLeft .GT. 1) THEN
    iLeft  = iLeft-1
    aValue = DataArray(iLeft)
    iValue = DataIndex(iLeft)
  ELSE
    aValue            = DataArray(iRight)
    iValue            = DataIndex(iRight)
    DataArray(iRight) = DataArray(1)
    DataIndex(iRight) = DataIndex(1)
    iRight            = iRight-1
    IF (iRight .EQ. 1) THEN
      DataArray(1) = aValue
      DataIndex(1) = iValue
      RETURN
    END IF
  END IF
  ii = iLeft
  jj = 2*iLeft
  DO WHILE (jj .LE. iRight)
    IF (jj .LT. iRight) THEN
      IF (DataArray(jj) .LT. DataArray(jj+1)) THEN
        jj = jj+1
      END IF
    END IF
    IF (aValue .LT. DataArray(jj)) THEN
      DataArray(ii) = DataArray(jj)
      DataIndex(ii) = DataIndex(jj)
      ii = jj
      jj = jj+jj
    ELSE
      jj = iRight+1
    END IF
  END DO
  DataArray(ii) = aValue
  DataIndex(ii) = iValue
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE HeapSort
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE HeapSortStruct(nNeighborsFound,SearchResults)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN)                   :: nNeighborsFound
TYPE(tKDTreeNeighbors),INTENT(INOUT) :: SearchResults(:)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: ii
INTEGER :: jj
INTEGER :: iLeft
INTEGER :: iRight
!----------------------------------------------------------------------------------------------------------------------!
TYPE(tKDTreeNeighbors) :: SearchResult
!----------------------------------------------------------------------------------------------------------------------!

iLeft  = nNeighborsFound/2+1
iRight = nNeighborsFound

IF (nNeighborsFound .EQ. 1) THEN
  RETURN
END IF

DO
  IF (iLeft .GT. 1) THEN
    iLeft = iLeft-1
    SearchResult = SearchResults(iLeft)
  ELSE
    SearchResult          = SearchResults(iRight)
    SearchResults(iRight) = SearchResults(1)
    iRight                = iRight-1
    IF (iRight .EQ. 1) THEN
      SearchResults(1) = SearchResult
      RETURN
    END IF
  END IF
  ii = iLeft
  jj = 2*iLeft
  DO WHILE (jj .LE. iRight)
    IF (jj .LT. iRight) THEN
      IF (SearchResults(jj)%Distance .LT. SearchResults(jj+1)%Distance) THEN
        jj = jj+1
      END IF
    END IF
    IF (SearchResult%Distance .LT. SearchResults(jj)%Distance) THEN
      SearchResults(ii) = SearchResults(jj)
      ii = jj
      jj = jj+jj
    ELSE
      jj = iRight+1
    END IF
  END DO
  SearchResults(ii) = SearchResult
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE HeapSortStruct
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_KDTreesBucketPointRegion
!======================================================================================================================!
