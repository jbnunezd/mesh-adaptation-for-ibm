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
#include<main.h>
!======================================================================================================================!
!
!======================================================================================================================!
MODULE MOD_PolynomialInterpolation
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE LegendrePolynomialAndDerivative
  MODULE PROCEDURE LegendrePolynomialAndDerivative
END INTERFACE

INTERFACE LegendreGaussNodesAndWeights
  MODULE PROCEDURE LegendreGaussNodesAndWeights
END INTERFACE

INTERFACE LegendreGaussLobattoNodesAndWeights
  MODULE PROCEDURE LegendreGaussLobattoNodesAndWeights
END INTERFACE

INTERFACE ChebyshevGaussNodesAndWeights
  MODULE PROCEDURE ChebyshevGaussNodesAndWeights
END INTERFACE

INTERFACE ChebyshevGaussLobattoNodesAndWeights
  MODULE PROCEDURE ChebyshevGaussLobattoNodesAndWeights
END INTERFACE

INTERFACE BarycentricWeights
  MODULE PROCEDURE BarycentricWeights
END INTERFACE

INTERFACE LagrangeInterpolation
  MODULE PROCEDURE LagrangeInterpolation
END INTERFACE

INTERFACE PolynomialInterpolationMatrix
  MODULE PROCEDURE PolynomialInterpolationMatrix
END INTERFACE

INTERFACE InterpolateToNewPoints
  MODULE PROCEDURE InterpolateToNewPoints
END INTERFACE

INTERFACE LagrangeInterpolatingPolynomials
  MODULE PROCEDURE LagrangeInterpolatingPolynomials
END INTERFACE

INTERFACE InterpolationCoarseToFine2D
  MODULE PROCEDURE InterpolationCoarseToFine2D
END INTERFACE

INTERFACE InterpolateToNewPoints1D
  MODULE PROCEDURE InterpolateToNewPoints1D
END INTERFACE

INTERFACE InterpolateToNewPoints2D
  MODULE PROCEDURE InterpolateToNewPoints2D
END INTERFACE

INTERFACE InterpolateToNewPoints3D
  MODULE PROCEDURE InterpolateToNewPoints3D
END INTERFACE

INTERFACE LagrangeInterpolationDerivative
  MODULE PROCEDURE LagrangeInterpolationDerivative
END INTERFACE

INTERFACE PolynomialDerivativeMatrix
  MODULE PROCEDURE PolynomialDerivativeMatrix
END INTERFACE

INTERFACE PolynomialDerivativeMatrix_HighOrder
  MODULE PROCEDURE PolynomialDerivativeMatrix_HighOrder
END INTERFACE

INTERFACE LegendrePolynomialInterpolationMatrix
  MODULE PROCEDURE LegendrePolynomialInterpolationMatrix
END INTERFACE

INTERFACE AlmostEqual
  MODULE PROCEDURE AlmostEqual
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: LegendrePolynomialAndDerivative
PUBLIC :: LegendreGaussNodesAndWeights
PUBLIC :: LegendreGaussLobattoNodesAndWeights
PUBLIC :: ChebyshevGaussNodesAndWeights
PUBLIC :: ChebyshevGaussLobattoNodesAndWeights
PUBLIC :: BarycentricWeights
PUBLIC :: LagrangeInterpolation
PUBLIC :: PolynomialInterpolationMatrix
PUBLIC :: InterpolateToNewPoints
PUBLIC :: LagrangeInterpolatingPolynomials
PUBLIC :: InterpolationCoarseToFine2D
PUBLIC :: InterpolateToNewPoints1D
PUBLIC :: InterpolateToNewPoints2D
PUBLIC :: InterpolateToNewPoints3D
PUBLIC :: LagrangeInterpolationDerivative
PUBLIC :: PolynomialDerivativeMatrix
PUBLIC :: PolynomialDerivativeMatrix_HighOrder
PUBLIC :: LegendrePolynomialInterpolationMatrix
PUBLIC :: AlmostEqual
!----------------------------------------------------------------------------------------------------------------------!
! Some local constants
!----------------------------------------------------------------------------------------------------------------------!
REAL,PARAMETER :: EPS = EPSILON(1.0D+00)
REAL,PARAMETER :: PI  = ACOS(-1.0)
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
SUBROUTINE LegendrePolynomialAndDerivative(N,x,LN,dLN)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 22
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: x
REAL,INTENT(OUT)   :: LN
REAL,INTENT(OUT)   :: dLN
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: k
REAL               :: LNm1, LNm2, dLNm1, dLNm2
!----------------------------------------------------------------------------------------------------------------------!

IF (N .EQ. 0) THEN
  LN  = 1.0
  dLN = 0.0
ELSEIF (N .EQ. 1) THEN
  LN  = x
  dLN = 1.0
ELSE
  LNm2  = 1.0
  LNm1  = x
  dLNm2 = 0.0
  dLNm1 = 1.0
  DO k=2,N
    LN    = (REAL(2*k-1)/REAL(k))*x*LNm1-(REAL(k-1)/REAL(k))*LNm2
    dLN   = dLNm2 + REAL(2*k-1)*LNm1
    LNm2  = LNm1
    LNm1  = LN
    dLNm2 = dLNm1
    dLNm1 = dLN
  END DO
END IF
LN  = LN*SQRT(REAL(N)+0.5)
dLN = dLN*SQRT(REAL(N)+0.5)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LegendrePolynomialAndDerivative
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE LegendreGaussNodesAndWeights(N,xNodes,xWeights)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 23
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(OUT)   :: xNodes(0:N)
REAL,INTENT(OUT)   :: xWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, k, nIter
REAL               :: LNp1, dLNp1, Delta
!----------------------------------------------------------------------------------------------------------------------!

nIter = 100
IF (N .EQ. 0) THEN
  xNodes(0)   = 0.0
  xWeights(0) = 2.0
ELSEIF (N .EQ. 1) THEN
  xNodes(0)   =-1.0/SQRT(3.0)
  xNodes(1)   =-xNodes(0)
  xWeights(0) = 1.0
  xWeights(1) = xWeights(0)
ELSE
  DO i=0,((N+1)/2-1)
    xNodes(i) = -COS((REAL(2*i+1)/REAL(2*N+2))*PI)
    DO k=0,nIter
      CALL LegendrePolynomialAndDerivative(N+1,xNodes(i),LNp1,dLNp1)
      Delta      =-LNp1/dLNp1
      xNodes(i) = xNodes(i) + Delta
      IF (ABS(Delta) .LE. 4*EPS*ABS(xNodes(i))) THEN
        EXIT
      END IF
    END DO
    CALL LegendrePolynomialAndDerivative(N+1,xNodes(i),LNp1,dLNp1)
    xNodes(N-i) = -xNodes(i)
    ! xWeights(i) = 2.0/((1.0-xNodes(i)**2)*(dLNp1)**2)
    xWeights(i) = REAL(2.0*N+3.0)/((1.0-xNodes(i)**2)*(dLNp1)**2)
    xWeights(N-i) = xWeights(i) 
  END DO
END IF

IF (MOD(N,2) .EQ. 0) THEN
  CALL LegendrePolynomialAndDerivative(N+1,0.0,LNp1,dLNp1)
  xNodes(N/2)   = 0.0
  ! xWeights(N/2) = 2.0/(dLNp1)**2
  xWeights(N/2) = REAL(2.0*N+3.0)/(dLNp1)**2
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LegendreGaussNodesAndWeights
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE qAndLEvaluation(N,x,q,dq,LN)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 24
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: x
REAL,INTENT(OUT)   :: q
REAL,INTENT(OUT)   :: dq
REAL,INTENT(OUT)   :: LN
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: k
REAL               :: dLN, LNm1, LNm2, dLNm1, dLNm2, LNp1, dLNp1
!----------------------------------------------------------------------------------------------------------------------!

k = 2
LNm2  = 1.0
LNm1  = x
dLNm2 = 0.0
dLNm1 = 1.0
DO k=2,N
  LN    = (REAL(2*k-1)/REAL(k))*x*LNm1-(REAL(k-1)/REAL(k))*LNm2
  dLN   = dLNm2+REAL(2*k-1)*LNm1
  LNm2  = LNm1
  LNm1  = LN
  dLNm2 = dLNm1
  dLNm1 = dLN
END DO
k = N+1
LNp1  = (REAL(2*k-1)/REAL(k))*x*LN - (REAL(k-1)/REAL(k))*LNm2
dLNp1 = dLNm2 + REAL(2*k-1)*LNm1
q     = LNp1-LNm2
dq    = dLNp1-dLNm2

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE qAndLEvaluation
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE LegendreGaussLobattoNodesAndWeights(N,xNodes,xWeights)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 25
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(OUT)   :: xNodes(0:N)
REAL,INTENT(OUT)   :: xWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, k, nIter
REAL               :: Delta, q, dq, LN
!----------------------------------------------------------------------------------------------------------------------!

nIter = 100
IF (N .EQ. 1) THEN
  xNodes(0)   =-1.0
  xWeights(0) = 1.0
  xNodes(1)   =-xNodes(0)
  xWeights(1) = xWeights(0)
ELSE
  xNodes(0)   =-1.0
  xWeights(0) = 2.0/REAL(N*(N+1))
  xNodes(N)   =-xNodes(0)
  xWeights(N) = xWeights(0)
  DO i=1,((N+1)/2-1)
    xNodes(i) = -COS((REAL(i+1.0/4.0)/REAL(N))*PI - (3.0/(8.0*N*PI))*(1.0/REAL(i+1.0/4.0)))
    DO k=0,nIter
      CALL qAndLEvaluation(N,xNodes(i),q,dq,LN)
      Delta = -q/dq
      xNodes(i) = xNodes(i) + Delta
      IF (ABS(Delta) .LE. 4*EPS*ABS(xNodes(i))) THEN
        EXIT
      END IF
    END DO
    CALL qAndLEvaluation(N,xNodes(i),q,dq,LN)
    xNodes(N-i)   = -xNodes(i)
    xWeights(i)   = 2.0/(REAL(N*(N+1))*LN**2)
    xWeights(N-i) = xWeights(i)
  END DO
END IF
IF (MOD(N,2) .EQ. 0) THEN
  CALL qAndLEvaluation(N,0.0,q,dq,LN)
  xNodes(N/2)   = 0.0
  xWeights(N/2) = 2.0/(REAL(N*(N+1))*LN**2)
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LegendreGaussLobattoNodesAndWeights
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ChebyshevGaussNodesAndWeights(N,xNodes,xWeights)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 26
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(OUT)   :: xNodes(0:N)
REAL,INTENT(OUT)   :: xWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i
!----------------------------------------------------------------------------------------------------------------------!

DO i=0,N
  xNodes(i)   = -COS((REAL(2*i+1)/REAL(2*N+2))*PI)
  xWeights(i) = PI/REAL(N+1)
  IF (AlmostEqual(xNodes(i),0.0) .EQV. .TRUE.) THEN
    xNodes(i) = 0.0
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ChebyshevGaussNodesAndWeights
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE ChebyshevGaussLobattoNodesAndWeights(N,xNodes,xWeights)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 27
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(OUT)   :: xNodes(0:N)
REAL,INTENT(OUT)   :: xWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i
!----------------------------------------------------------------------------------------------------------------------!

DO i=0,N
  xNodes(i)   = -COS((REAL(i)/REAL(N))*PI)
  xWeights(i) = PI/REAL(N)
  IF (AlmostEqual(xNodes(i),0.0) .EQV. .TRUE.) THEN
    xNodes(i) = 0.0
  END IF
END DO
xWeights(0) = xWeights(0)/2.0
xWeights(N) = xWeights(N)/2.0

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE ChebyshevGaussLobattoNodesAndWeights
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BarycentricWeights(N,xNodes,xBaryWeights)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 30
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(OUT)   :: xBaryWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, j
!----------------------------------------------------------------------------------------------------------------------!

DO i=0,N
  xBaryWeights(i) = 1.0
END DO
DO i=1,N
  DO j=0,i-1
    xBaryWeights(j) = xBaryWeights(j)*(xNodes(j)-xNodes(i))
    xBaryWeights(i) = xBaryWeights(i)*(xNodes(i)-xNodes(j))
  END DO
END DO
DO i=0,N
  xBaryWeights(i) = 1.0/xBaryWeights(i)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BarycentricWeights
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE LagrangeInterpolation(N,xNodes,xBaryWeights,U1D,x,Lx)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 31
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(IN)    :: xBaryWeights(0:N)
REAL,INTENT(IN)    :: U1D(0:N)
REAL,INTENT(IN)    :: x
REAL,INTENT(OUT)   :: Lx
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i
REAL               :: num, dem, t
!----------------------------------------------------------------------------------------------------------------------!

num = 0.0
dem = 0.0
DO i=0,N
  IF (AlmostEqual(x,xNodes(i)) .EQV. .TRUE.) THEN
    Lx = U1D(i)
    RETURN
  END IF
  t = xBaryWeights(i)/(x-xNodes(i))
  num = num + t*U1D(i)
  dem = dem + t
END DO
Lx = num/dem

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LagrangeInterpolation
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PolynomialInterpolationMatrix(N_old,N_new,xNodes_old,xBaryWeights,xNodes_new,VDM)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 32
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N_old
INTEGER,INTENT(IN) :: N_new
REAL,INTENT(IN)    :: xNodes_old(0:N_old)
REAL,INTENT(IN)    :: xBaryWeights(0:N_old)
REAL,INTENT(IN)    :: xNodes_new(0:N_new)
REAL,INTENT(OUT)   :: VDM(0:N_new,0:N_old)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: k, j
REAL               :: s, t
LOGICAL            :: rowHasMatch
!----------------------------------------------------------------------------------------------------------------------!

DO k=0,N_new
  rowHasMatch = .FALSE.
  DO j=0,N_old
    VDM(k,j) = 0.0
    IF (AlmostEqual(xNodes_new(k),xNodes_old(j))) THEN
      rowHasMatch = .TRUE.
      VDM(k,j) = 1.0
    END IF
  END DO
  IF (rowHasMatch .EQV. .FALSE.) THEN
    s = 0.0
    DO j=0,N_old
      t = xBaryWeights(j)/(xNodes_new(k)-xNodes_old(j))
      VDM(k,j) = t
      s = s + t
    END DO
    DO j=0,N_old
      VDM(k,j) = VDM(k,j)/s
    END DO
  END IF
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PolynomialInterpolationMatrix
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints(N_old,N_new,VDM,U1D_old,U1D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 33
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N_old
INTEGER,INTENT(IN) :: N_new
REAL,INTENT(IN)    :: VDM(0:N_new,0:N_old)
REAL,INTENT(IN)    :: U1D_old(0:N_old)
REAL,INTENT(OUT)   :: U1D_new(0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i,j
REAL               :: t
!----------------------------------------------------------------------------------------------------------------------!

DO i=0,N_new
  t = 0.0
  DO j=0,N_old
    t = t + VDM(i,j)*U1D_old(j)
  END DO
  U1D_new(i) = t
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE LagrangeInterpolatingPolynomials(N,xNodes,xWeights,x,L)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 34
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(IN)    :: xWeights(0:N)
REAL,INTENT(IN)    :: x
REAL,INTENT(OUT)   :: L(0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i
REAL               :: s, t
LOGICAL            :: xMatchesNode
!----------------------------------------------------------------------------------------------------------------------!

xMatchesNode = .FALSE.
DO i=0,N
  L(i) = 0.0
  IF (AlmostEqual(x,xNodes(i)) .EQV. .TRUE.) THEN
    L(i) = 1.0
    xMatchesNode = .TRUE.
  END IF
END DO

IF (xMatchesNode .EQV. .TRUE.) THEN
  RETURN
END IF

s = 0.0
DO i=0,N
  t    = xWeights(i)/(x-xNodes(i))
  L(i) = t
  s    = s+t
END DO
DO i=0,N
  L(i) = L(i)/s
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LagrangeInterpolatingPolynomials
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolationCoarseToFine2D(&
  N_old,M_old,N_new,M_new,xNodes_old,yNodes_old,xNodes_new,yNodes_new,U2D_old,U2D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N_old
INTEGER,INTENT(IN) :: M_old
INTEGER,INTENT(IN) :: N_new
INTEGER,INTENT(IN) :: M_new
REAL,INTENT(IN)    :: xNodes_old(0:N_old)
REAL,INTENT(IN)    :: yNodes_old(0:M_old)
REAL,INTENT(IN)    :: xNodes_new(0:N_new)
REAL,INTENT(IN)    :: yNodes_new(0:M_new)
REAL,INTENT(IN)    :: U2D_old(0:N_old,0:M_old)
REAL,INTENT(OUT)   :: U2D_new(0:N_new,0:M_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: j, n
REAL               :: xBaryWeights(0:N_old)
REAL               :: yBarycentricWeights(0:M_old)
REAL               :: VDM1(0:N_new,0:N_old)
REAL               :: VDM2(0:M_new,0:M_old)
REAL               :: fxy_temp(0:N_new,0:M_old)
!----------------------------------------------------------------------------------------------------------------------!

CALL BarycentricWeights(N_old,xNodes_old,xBaryWeights)
CALL PolynomialInterpolationMatrix(N_old,N_new,xNodes_old,xBaryWeights,xNodes_new,VDM1)
DO j=0,M_old
  CALL InterpolateToNewPoints(N_old,N_new,VDM1,U2D_old(0:N_old,j),fxy_temp(0:N_new,j))
END DO

CALL BarycentricWeights(M_old,yNodes_old(0:M_old),yBarycentricWeights)
CALL PolynomialInterpolationMatrix(M_old,M_new,yNodes_old,yBarycentricWeights,yNodes_new,VDM2)
DO n=0,N_new
  CALL InterpolateToNewPoints(M_old,M_new,VDM2,fxy_temp(n,0:M_old),U2D_new(n,0:M_new))
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolationCoarseToFine2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints1D(nVar,N_old,N_new,VDM,U1D_old,U1D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35 - Altern
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nVar
INTEGER,INTENT(IN) :: N_old
INTEGER,INTENT(IN) :: N_new
REAL,INTENT(IN)    :: VDM(0:N_new,0:N_old)
REAL,INTENT(IN)    :: U1D_old(1:nVar,0:N_old)
REAL,INTENT(OUT)   :: U1D_new(1:nVar,0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i_old, i_new
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VDM,2) .NE. SIZE(U1D_old,2)) THEN
  ErrorMessage = "Dimension incompatibility between VDM and Uin"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF
IF (SIZE(U1D_old,1) .NE. SIZE(U1D_new,1)) THEN
  ErrorMessage = "Dimension incompatibility between Uin and Uout"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

U1D_new = 0.0
DO i_old=0,N_old
  DO i_new=0,N_new
    U1D_new(:,i_new) = U1D_new(:,i_new) + VDM(i_new,i_old)*U1D_old(:,i_old)
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints1D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints2D(nVar,N_old,N_new,VDM,U2D_old,U2D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35 - Altern
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nVar
INTEGER,INTENT(IN) :: N_old
INTEGER,INTENT(IN) :: N_new
REAL,INTENT(IN)    :: VDM(0:N_new,0:N_old)
REAL,INTENT(IN)    :: U2D_old(1:nVar,0:N_old,0:N_old)
REAL,INTENT(OUT)   :: U2D_new(1:nVar,0:N_new,0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i_old, i_new
INTEGER            :: j_old, j_new
REAL               :: U2D_buffer(1:nVar,0:N_new,0:N_old)
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VDM,2) .NE. SIZE(U2D_old,2)) THEN
  ErrorMessage = "Dimension incompatibility between VDM and Uin"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF
IF (SIZE(U2D_old,1) .NE. SIZE(U2D_new,1)) THEN
  ErrorMessage = "Dimension incompatibility between Uin and Uout"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

U2D_buffer = 0.0
DO j_old=0,N_old
  DO i_old=0,N_old
    DO i_new=0,N_new
      U2D_buffer(:,i_new,j_old) = U2D_buffer(:,i_new,j_old) &
                                + VDM(i_new,i_old)*U2D_old(:,i_old,j_old)
    END DO
  END DO
END DO

U2D_new = 0.0
DO j_old=0,N_old
  DO j_new=0,N_new
    DO i_new=0,N_new
      U2D_new(:,i_new,j_new) = U2D_new(:,i_new,j_new) &
                             + VDM(j_new,j_old)*U2D_buffer(:,i_new,j_old)
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints2D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints3D(nVar,N_old,N_new,VDM,U3D_old,U3D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35 - Altern
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: nVar
INTEGER,INTENT(IN) :: N_old
INTEGER,INTENT(IN) :: N_new
REAL,INTENT(IN)    :: VDM(0:N_new,0:N_old)
REAL,INTENT(IN)    :: U3D_old(1:nVar,0:N_old,0:N_old,0:N_old)
REAL,INTENT(OUT)   :: U3D_new(1:nVar,0:N_new,0:N_new,0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i_old, i_new
INTEGER            :: j_old, j_new
INTEGER            :: k_old, k_new
REAL               :: U3D_buffer1(1:nVar,0:N_new,0:N_old,0:N_old)
REAL               :: U3D_buffer2(1:nVar,0:N_new,0:N_new,0:N_old)
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VDM,2) .NE. SIZE(U3D_old,2)) THEN
  ErrorMessage = "Dimension incompatibility between VDM and Uin"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF
IF (SIZE(U3D_old,1) .NE. SIZE(U3D_new,1)) THEN
  ErrorMessage = "Dimension incompatibility between Uin and Uout"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

U3D_buffer1 = 0.0
DO k_old=0,N_old
  DO j_old=0,N_old
    DO i_old=0,N_old
      DO i_new=0,N_new
        U3D_buffer1(:,i_new,j_old,k_old) = U3D_buffer1(:,i_new,j_old,k_old) &
                                         + VDM(i_new,i_old)*U3D_old(:,i_old,j_old,k_old)
      END DO
    END DO
  END DO
END DO

U3D_buffer2 = 0.0
DO k_old=0,N_old
  DO j_old=0,N_old
    DO j_new=0,N_new
      DO i_new=0,N_new
        U3D_buffer2(:,i_new,j_new,k_old) = U3D_buffer2(:,i_new,j_new,k_old) &
                                         + VDM(j_new,j_old)*U3D_buffer1(:,i_new,j_old,k_old)
      END DO
    END DO
  END DO
END DO

U3D_new = 0.0
DO k_old=0,N_old
  DO k_new=0,N_new
    DO j_new=0,N_new
      DO i_new=0,N_new
        U3D_new(:,i_new,j_new,k_new) = U3D_new(:,i_new,j_new,k_new) &
                                     + VDM(k_new,k_old)*U3D_buffer2(:,i_new,j_new,k_old)
      END DO
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints1D_OLD(VDM,U1D_old,U1D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35 - Altern
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)              :: VDM(:,:)      ! VDM(0:N_new,0:N_old)
REAL,INTENT(IN)              :: U1D_old(:,:)  ! U1D_old(1:nVar,0:N_old)
REAL,INTENT(OUT),ALLOCATABLE :: U1D_new(:,:)  ! U1D_new(1:nVar,0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i_old, i_new
INTEGER            :: nVar, N_old, N_new
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VDM,2) .NE. SIZE(U1D_old,2)) THEN
  ErrorMessage = "Dimension incompatibility between VDM and Uin"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF
IF (SIZE(U1D_old,1) .NE. SIZE(U1D_new,1)) THEN
  ErrorMessage = "Dimension incompatibility between Uin and Uout"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

N_old = SIZE(VDM,2)
N_new = SIZE(VDM,1)
nVar  = SIZE(U1D_old,1)
ALLOCATE(U1D_new(1:nVar,1:N_new))

U1D_new = 0.0
DO i_old=1,N_old
  DO i_new=1,N_new
    U1D_new(:,i_new) = U1D_new(:,i_new) + VDM(i_new,i_old)*U1D_old(:,i_old)
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints1D_OLD
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints2D_OLD(VDM,U2D_old,U2D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35 - Altern
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)              :: VDM(:,:)        ! VDM(0:N_new,0:N_old)
REAL,INTENT(IN)              :: U2D_old(:,:,:)  ! U2D_old(1:nVar,0:N_old,0:N_old)
REAL,INTENT(OUT),ALLOCATABLE :: U2D_new(:,:,:)  ! U2D_new(1:nVar,0:N_new,0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i_old, i_new
INTEGER            :: j_old, j_new
INTEGER            :: nVar, N_old, N_new
REAL,ALLOCATABLE   :: U2D_buffer(:,:,:)
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VDM,2) .NE. SIZE(U2D_old,2)) THEN
  ErrorMessage = "Dimension incompatibility between VDM and Uin"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF
IF (SIZE(U2D_old,1) .NE. SIZE(U2D_new,1)) THEN
  ErrorMessage = "Dimension incompatibility between Uin and Uout"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

N_old = SIZE(VDM,2)
N_new = SIZE(VDM,1)
nVar  = SIZE(U2D_old,1)
ALLOCATE(U2D_new(1:nVar,1:N_new,1:N_new))
ALLOCATE(U2D_buffer(1:nVar,1:N_new,1:N_old))

U2D_buffer = 0.0
DO j_old=1,N_old
  DO i_old=1,N_old
    DO i_new=1,N_new
      U2D_buffer(:,i_new,j_old) = U2D_buffer(:,i_new,j_old) &
                                + VDM(i_new,i_old)*U2D_old(:,i_old,j_old)
    END DO
  END DO
END DO

U2D_new = 0.0
DO j_old=1,N_old
  DO j_new=1,N_new
    DO i_new=1,N_new
      U2D_new(:,i_new,j_new) = U2D_new(:,i_new,j_new) &
                             + VDM(j_new,j_old)*U2D_buffer(:,i_new,j_old)
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints2D_OLD
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InterpolateToNewPoints3D_OLD(VDM,U3D_old,U3D_new)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 35 - Altern
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)              :: VDM(:,:)         ! VDM(0:N_new,0:N_old)
REAL,INTENT(IN)              :: U3D_old(:,:,:,:) ! U3D_old(1:nVar,0:N_old,0:N_old,0:N_old)
REAL,INTENT(OUT),ALLOCATABLE :: U3D_new(:,:,:,:) ! U3D_new(1:nVar,0:N_new,0:N_new,0:N_new)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i_old, i_new
INTEGER            :: j_old, j_new
INTEGER            :: k_old, k_new
INTEGER            :: nVar, N_old, N_new
REAL,ALLOCATABLE   :: U3D_buffer1(:,:,:,:)
REAL,ALLOCATABLE   :: U3D_buffer2(:,:,:,:)
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

IF (SIZE(VDM,2) .NE. SIZE(U3D_old,2)) THEN
  ErrorMessage = "Dimension incompatibility between VDM and Uin"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF
IF (SIZE(U3D_old,1) .NE. SIZE(U3D_new,1)) THEN
  ErrorMessage = "Dimension incompatibility between Uin and Uout"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

N_old = SIZE(VDM,2)
N_new = SIZE(VDM,1)
nVar  = SIZE(U3D_old,1)
ALLOCATE(U3D_new(1:nVar,1:N_new,1:N_new,1:N_new))
ALLOCATE(U3D_buffer1(1:nVar,1:N_new,1:N_old,1:N_old))
ALLOCATE(U3D_buffer2(1:nVar,1:N_new,1:N_new,1:N_old))

U3D_buffer1 = 0.0
DO k_old=1,N_old
  DO j_old=1,N_old
    DO i_old=1,N_old
      DO i_new=1,N_new
        U3D_buffer1(:,i_new,j_old,k_old) = U3D_buffer1(:,i_new,j_old,k_old) &
                                         + VDM(i_new,i_old)*U3D_old(:,i_old,j_old,k_old)
      END DO
    END DO
  END DO
END DO

U3D_buffer2 = 0.0
DO k_old=1,N_old
  DO j_old=1,N_old
    DO j_new=1,N_new
      DO i_new=1,N_new
        U3D_buffer2(:,i_new,j_new,k_old) = U3D_buffer2(:,i_new,j_new,k_old) &
                                         + VDM(j_new,j_old)*U3D_buffer1(:,i_new,j_old,k_old)
      END DO
    END DO
  END DO
END DO

U3D_new = 0.0
DO k_old=1,N_old
  DO k_new=1,N_new
    DO j_new=1,N_new
      DO i_new=1,N_new
        U3D_new(:,i_new,j_new,k_new) = U3D_new(:,i_new,j_new,k_new) &
                                     + VDM(k_new,k_old)*U3D_buffer2(:,i_new,j_new,k_old)
      END DO
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InterpolateToNewPoints3D_OLD
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE LagrangeInterpolationDerivative(N,xNodes,xBaryWeights,U1D,x,dU1Dx)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 36
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(IN)    :: xBaryWeights(0:N)
REAL,INTENT(IN)    :: U1D(0:N)
REAL,INTENT(IN)    :: x
REAL,INTENT(OUT)   :: dU1Dx
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, j
REAL               :: num, den, t, Lx
LOGICAL            :: atNode
!----------------------------------------------------------------------------------------------------------------------!

atNode = .FALSE.
num = 0.0

DO j= 0,N
  IF (AlmostEqual(x,xNodes(j)) .EQV. .TRUE.) THEN
    atNode = .TRUE.
    Lx = U1D(j)
    den = -xBaryWeights(j)
    i = j
  END IF
END DO
IF (atNode .EQV. .TRUE.) THEN
  DO j=0,N
    IF (j .NE. i) THEN
      num = num + xBaryWeights(j)*(Lx-U1D(j))/(x-xNodes(j))
    END IF
  END DO
ELSE
  den = 0.0
  CALL LagrangeInterpolation(N,xNodes,xBaryWeights,U1D,x,Lx)
  DO j=0,N
    t = xBaryWeights(j)/(x-xNodes(j))
    num = num + t*(Lx-U1D(j))/(x-xNodes(j))
    den = den + t
  END DO
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LagrangeInterpolationDerivative
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PolynomialDerivativeMatrix(N,xNodes,DMatrix)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 37
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(OUT)   :: DMatrix(0:N,0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, j
REAL               :: xBaryWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!

CALL BarycentricWeights(N,xNodes,xBaryWeights)

DO i=0,N
  DMatrix(i,i) = 0.0
  DO j=0,N
    IF (j .NE. i) THEN
      DMatrix(i,j) = (xBaryWeights(j)/xBaryWeights(i))/(xNodes(i)-xNodes(j))
      DMatrix(i,i) = DMatrix(i,i) - DMatrix(i,j)
    END IF
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PolynomialDerivativeMatrix
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE PolynomialDerivativeMatrix_HighOrder(N,xNodes,DMatrix)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 38
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(OUT)   :: DMatrix(0:N,0:N,0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, j, k
REAL               :: xBaryWeights(0:N)
!----------------------------------------------------------------------------------------------------------------------!

DMatrix = 0.0
CALL BarycentricWeights(N,xNodes,xBaryWeights)
CALL PolynomialDerivativeMatrix(N,xNodes,DMatrix(:,:,1))

DO k=2,N
  DO i=0,N
    DMatrix(i,i,k) = 0.0
    DO j=0,N
      IF (j .NE. i) THEN
        DMatrix(i,j,k) = (REAL(k)/(xNodes(i)-xNodes(j)))*&
                      ((xBaryWeights(j)/xBaryWeights(i))*DMatrix(i,i,k-1)-DMatrix(i,j,k))
        DMatrix(i,i,k) = DMatrix(i,i,k) - DMatrix(i,j,k)
      END IF
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE PolynomialDerivativeMatrix_HighOrder
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE LegendrePolynomialInterpolationMatrix(N,xNodes,VDM,invVDM)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: N
REAL,INTENT(IN)    :: xNodes(0:N)
REAL,INTENT(OUT)   :: VDM(0:N,0:N)
REAL,INTENT(OUT)   :: invVDM(0:N,0:N)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER            :: i, j
REAL               :: temp
REAL               :: xGaussNodes(0:N)
REAL               :: xGaussWeights(0:N)
REAL               :: xBaryWeights(0:N)
REAL,PARAMETER     :: EPS = 1.0E-15
CHARACTER(LEN=255) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

CALL BarycentricWeights(N,xNodes,xBaryWeights)
CALL LegendreGaussNodesAndWeights(N,xGaussNodes,xGaussWeights)

DO i=0,N
  DO j=0,N
    CALL LegendrePolynomialAndDerivative(j,xGaussNodes(i),VDM(i,j),temp)
  END DO
END DO
VDM = TRANSPOSE(VDM)
DO j=0,N
  VDM(:,j) = VDM(:,j)*xGaussWeights(j)
END DO

CALL PolynomialInterpolationMatrix(N,N,xNodes,xBaryWeights,xGaussNodes,invVDM)
invVDM = MATMUL(VDM,invVDM)

DO i=0,N
  DO j=0,N
    CALL LegendrePolynomialAndDerivative(j,xNodes(i),VDM(i,j),temp)
  END DO
END DO

temp = SUM((ABS(MATMUL(invVDM,VDM)))-(N+1))
IF (temp .GT. EPS) THEN
  ErrorMessage = "Problems in MODAL <-> NODAL Vandermonde Matrix"
  WRITE(UNIT_SCREEN,*) TRIM(ErrorMessage)
  STOP
END IF

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE LegendrePolynomialInterpolationMatrix
!======================================================================================================================!
!
!
!
!======================================================================================================================!
FUNCTION AlmostEqual(x1,x2)
!----------------------------------------------------------------------------------------------------------------------!
! Kopriva2009Book: Algorithm 139
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN) :: x1
REAL,INTENT(IN) :: x2
LOGICAL         :: AlmostEqual
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

AlmostEqual = .FALSE.
IF ((x1 .EQ. 0.0) .OR. (x2 .EQ. 0.0)) THEN
  IF (ABS(x1-x2) .LE. 2.0*EPS) THEN
    AlmostEqual = .TRUE.
  ELSE
    AlmostEqual = .FALSE.
  END IF
ELSE
  IF ((ABS(x1-x2) .LE. EPS*ABS(x1)) .AND. (ABS(x1-x2) .LE. EPS*ABS(x2))) THEN
    AlmostEqual = .TRUE.
  ELSE
    AlmostEqual = .FALSE.
  END IF
END IF

!----------------------------------------------------------------------------------------------------------------------!
END FUNCTION AlmostEqual
!======================================================================================================================!
!
!
!
!======================================================================================================================!
END MODULE MOD_PolynomialInterpolation
!----------------------------------------------------------------------------------------------------------------------!
