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
MODULE MOD_PolynomialRoots
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE QuarticRoots
  MODULE PROCEDURE QuarticRoots
END INTERFACE

INTERFACE CubicRoots
  MODULE PROCEDURE CubicRoots
END INTERFACE

INTERFACE QuadraticRoots
  MODULE PROCEDURE QuadraticRoots
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: QuarticRoots
PUBLIC :: CubicRoots
PUBLIC :: QuadraticRoots
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
SUBROUTINE QuarticRoots(q3,q2,q1,q0,nReal,root)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)     :: q3
REAL,INTENT(IN)     :: q2
REAL,INTENT(IN)     :: q1
REAL,INTENT(IN)     :: q0
INTEGER,INTENT(OUT) :: nReal
REAL,INTENT(OUT)    :: root(1:4,1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL           :: bisection
LOGICAL           :: converged
LOGICAL           :: iterate
LOGICAL           :: minimum
LOGICAL           :: notZero
INTEGER           :: deflateCase
INTEGER           :: oscillate
INTEGER           :: quarticType
INTEGER,PARAMETER :: Re = 1
INTEGER,PARAMETER :: Im = 2
INTEGER,PARAMETER :: biquadratic = 2
INTEGER,PARAMETER :: cubic       = 3
INTEGER,PARAMETER :: general     = 4
REAL              :: a0, a1, a2, a3
REAL              :: a, b, c, d, k, s, t, u, x, y, z
REAL,PARAMETER    :: macheps = EPSILON(1.0D0)
REAL,PARAMETER    :: third   = 1.0/3.0
!----------------------------------------------------------------------------------------------------------------------!

! Start.

! Handle special cases. Since the cubic solver handles all its
! special cases by itself, we need to check only for two cases:
! 1) independent term is zero -> solve cubic and include the zero root
! 2) the biquadratic case.
IF (q0 == 0.0) THEN
  k  = 1.0
  a3 = q3
  a2 = q2
  a1 = q1
  quarticType = cubic
ELSE IF (q3 == 0.0 .and. q1 == 0.0) THEN
  k  = 1.0
  a2 = q2
  a0 = q0
  quarticType = biquadratic
ELSE
  ! The general case. Rescale quartic polynomial, such that largest absolute coefficient
  ! is (exactly!) equal to 1. Honor the presence of a special quartic case that might have
  ! been obtained during the rescaling process (due to underflow in the coefficients).
  s = ABS(q3)
  t = SQRT(ABS(q2))
  u = ABS(q1)**third
  x = SQRT(SQRT(ABS(q0)))
  y = MAX(s,t,u,x)
  IF (y == s) THEN
    k  = 1.0 / s
    a3 = SIGN(1.0 , q3)
    a2 = (q2 * k) * k
    a1 = ((q1 * k) * k) * k
    a0 = (((q0 * k) * k) * k) * k
  ELSE IF (y == t) THEN
    k  = 1.0 / t
    a3 = q3 * k
    a2 = SIGN(1.0 , q2)
    a1 = ((q1 * k) * k) * k
    a0 = (((q0 * k) * k) * k) * k
  ELSE IF (y == u) THEN
    k  = 1.0 / u
    a3 = q3 * k
    a2 = (q2 * k) * k
    a1 = SIGN(1.0 , q1)
    a0 = (((q0 * k) * k) * k) * k
  ELSE
    k  = 1.0 / x
    a3 = q3 * k
    a2 = (q2 * k) * k
    a1 = ((q1 * k) * k) * k
    a0 = SIGN(1.0 , q0)
  END IF

  k = 1.0 / k
  IF (a0 == 0.0) THEN
    quarticType = cubic
  ELSE IF (a3 == 0.0 .and. a1 == 0.0) THEN
    quarticType = biquadratic
  ELSE
    quarticType = general
  END IF
END IF

! Select the case.
SELECT CASE (quarticType)
  ! 1) The quartic with independent term = 0 -> solve cubic and add a zero root.
  CASE(cubic)
    CALL CubicRoots(a3, a2, a1, nReal, root (1:3,1:2))
    IF (nReal == 3) THEN
      x = root (1,Re) * k       ! real roots of cubic are ordered x >= y >= z
      y = root (2,Re) * k
      z = root (3,Re) * k

      nReal = 4
      root (1,Re) = MAX(x, 0.0)
      root (2,Re) = MAX(y, MIN(x, 0.0))
      root (3,Re) = MAX(z, MIN(y, 0.0))
      root (4,Re) = MIN(z, 0.0)
      root (:,Im) = 0.0
    ELSE                          ! there is only one real cubic root here
      x = root (1,Re) * k

      nReal = 2
      root (4,Re) = root (3,Re) * k
      root (3,Re) = root (2,Re) * k
      root (2,Re) = MIN(x, 0.0)
      root (1,Re) = MAX(x, 0.0)

      root (4,Im) = root (3,Im) * k
      root (3,Im) = root (2,Im) * k
      root (2,Im) = 0.0
      root (1,Im) = 0.0
    END IF
  ! 2) The quartic with x^3 and x terms = 0 -> solve biquadratic.
  CASE(biquadratic)
    CALL QuadraticRoots(q2, q0, nReal, root (1:2,1:2))
    IF (nReal == 2) THEN
      x = root (1,Re)         ! real roots of quadratic are ordered x >= y
      y = root (2,Re)
      IF (y >= 0.0) THEN
        x = SQRT(x) * k
        y = SQRT(y) * k

        nReal = 4
        root (1,Re) = x
        root (2,Re) = y
        root (3,Re) = - y
        root (4,Re) = - x
        root (:,Im) = 0.0
      ELSE IF (x >= 0.0 .and. y < 0.0) THEN
        x = SQRT(x)       * k
        y = SQRT(ABS(y)) * k

        nReal = 2
        root (1,Re) = x
        root (2,Re) = - x
        root (3,Re) = 0.0
        root (4,Re) = 0.0
        root (1,Im) = 0.0
        root (2,Im) = 0.0
        root (3,Im) = y
        root (4,Im) = - y
      ELSE IF (x < 0.0) THEN
        x = SQRT(ABS(x)) * k
        y = SQRT(ABS(y)) * k

        nReal = 0
        root (:,Re) = 0.0
        root (1,Im) = y
        root (2,Im) = x
        root (3,Im) = - x
        root (4,Im) = - y
      END IF
    ELSE          ! complex conjugate pair biquadratic roots x +/- iy.
      x = root (1,Re) * 0.5
      y = root (1,Im) * 0.5
      z = SQRT(x * x + y * y)
      y = SQRT(z - x) * k
      x = SQRT(z + x) * k

      nReal = 0
      root (1,Re) = x
      root (2,Re) = x
      root (3,Re) = - x
      root (4,Re) = - x
      root (1,Im) = y
      root (2,Im) = - y
      root (3,Im) = y
      root (4,Im) = - y
    END IF

  ! 3) The general quartic case. Search for stationary points. Set the first
  !    derivative polynomial (cubic) equal to zero and find its roots.
  !    Check, if any minimum point of Q(x) is below zero, in which case we
  !    must have real roots for Q(x). Hunt down only the real root, which
  !    will potentially converge fastest during Newton iterates. The remaining
  !    roots will be determined by deflation Q(x) -> cubic.
  !    The best roots for the Newton iterations are the two on the opposite
  !    ends, i.e. those closest to the +2 and -2. Which of these two roots
  !    to take, depends on the location of the Q(x) minima x = s and x = u,
  !    with s > u. There are three cases:
  !    1) both Q(s) and Q(u) < 0
  !       ----------------------
  !       The best root is the one that corresponds to the lowest of
  !       these minima. If Q(s) is lowest -> start Newton from +2
  !       downwards (or zero, if s < 0 and a0 > 0). If Q(u) is lowest
  !       -> start Newton from -2 upwards (or zero, if u > 0 and a0 > 0).
  !    2) only Q(s) < 0
  !       -------------
  !       With both sides +2 and -2 possible as a Newton starting point,
  !       we have to avoid the area in the Q(x) graph, where inflection
  !       points are present. Solving Q''(x) = 0, leads to solutions
  !       x = -a3/4 +/- discriminant, i.e. they are centered around -a3/4.
  !       Since both inflection points must be either on the r.h.s or l.h.s.
  !       from x = s, a simple test where s is in relation to -a3/4 allows
  !       us to avoid the inflection point area.
  !    3) only Q(u) < 0
  !       -------------
  !       Same of what has been said under 2) but with x = u.
  CASE(general)
    x = 0.75 * a3
    y = 0.50 * a2
    z = 0.25 * a1

    CALL CubicRoots(x, y, z, nReal, root (1:3,1:2))
    s = root (1,Re)        ! Q'(x) root s (real for sure)
    x = s + a3
    x = x * s + a2
    x = x * s + a1
    x = x * s + a0         ! Q(s)

    y = 1.0             ! dual info: Q'(x) has more real roots, and if so, is Q(u) < 0 ? 
    IF (nReal > 1) THEN
      u = root (3,Re)    ! Q'(x) root u
      y = u + a3
      y = y * u + a2
      y = y * u + a1
      y = y * u + a0     ! Q(u)
    END IF
    IF (x < 0.0 .and. y < 0.0) THEN
      IF (x < y) THEN
        IF (s < 0.0) THEN
          x = 1.0 - SIGN(1.0,a0)
        ELSE
          x = 2.0
        END IF
      ELSE
        IF (u > 0.0) THEN
          x = - 1.0 + SIGN(1.0,a0)
        ELSE
          x = - 2.0
        END IF
      END IF
      nReal = 1
    ELSE IF (x < 0.0) THEN
      IF (s < - a3 * 0.25) THEN
        IF (s > 0.0) THEN
          x = - 1.0 + SIGN(1.0,a0)
        ELSE
          x = - 2.0
        END IF
      ELSE
        IF (s < 0.0) THEN
          x = 1.0 - SIGN(1.0,a0)
        ELSE
          x = 2.0
        END IF
      END IF
      nReal = 1
    ELSE IF (y < 0.0) THEN
      IF (u < - a3 * 0.25) THEN
          IF (u > 0.0) THEN
              x = - 1.0 + SIGN(1.0,a0)
          ELSE
              x = - 2.0
          END IF
      ELSE
          IF (u < 0.0) THEN
              x = 1.0 - SIGN(1.0,a0)
          ELSE
              x = 2.0
          END IF
      END IF
      nReal = 1
    ELSE
      nReal = 0
    END IF
    ! Do all necessary Newton iterations. In case we have more than 2 oscillations,
    ! exit the Newton iterations and switch to bisection. Note, that from the
    ! definition of the Newton starting point, we always have Q(x) > 0 and Q'(x)
    ! starts (-ve/+ve) for the (-2/+2) starting points and (increase/decrease) smoothly
    ! and staying (< 0 / > 0). In practice, for extremely shallow Q(x) curves near the
    ! root, the Newton procedure can overshoot slightly due to rounding errors when
    ! approaching the root. The result are tiny oscillations around the root. If such
    ! a situation happens, the Newton iterations are abandoned after 3 oscillations
    ! and further location of the root is done using bisection starting with the
    ! oscillation brackets.
    IF (nReal > 0) THEN
      oscillate = 0
      bisection = .false.
      converged = .false.
      DO WHILE (.not.converged .and. .not.bisection)    ! Newton-Raphson iterates
        y = x + a3                                     !
        z = x + y                                      !
        y = y * x + a2                                 ! y = Q(x)
        z = z * x + y                                  !
        y = y * x + a1                                 ! z = Q'(x)
        z = z * x + y                                  !
        y = y * x + a0                                 !
        IF (y < 0.0) THEN                           ! does Newton start oscillating ?
            oscillate = oscillate + 1                  ! increment oscillation counter
            s = x                                      ! save lower bisection bound
        ELSE
            u = x                                      ! save upper bisection bound
        END IF
        y = y / z                                      ! Newton correction
        x = x - y                                      ! new Newton root
        bisection = oscillate > 2                      ! activate bisection
        converged = ABS(y) <= ABS(x) * macheps       ! Newton convergence indicator
      END DO
      IF (bisection) THEN
        t = u - s                                     ! initial bisection interval
        DO WHILE (ABS(t) > ABS(x) * macheps)        ! bisection iterates
          y = x + a3                                 !
          y = y * x + a2                             ! y = Q(x)
          y = y * x + a1                             !
          y = y * x + a0                             !
          IF (y < 0.0) THEN                       !
              s = x                                  !
          ELSE                                       ! keep bracket on root
              u = x                                  !
          END IF                                     !
          t = 0.5 * (u - s)                       ! new bisection interval
          x = s + t                                  ! new bisection root
        END DO
      END IF

      ! Find remaining roots -> reduce to cubic. The reduction to a cubic polynomial
      ! is done using composite deflation to minimize rounding errors. Also, while
      ! the composite deflation analysis is done on the reduced quartic, the actual
      ! deflation is being performed on the original quartic again to avoid enhanced
      ! propagation of root errors.
      z = ABS(x)            !
      a = ABS(a3)           !
      b = ABS(a2)           ! prepare for composite deflation
      c = ABS(a1)           !
      d = ABS(a0)           !

      y = z * MAX(a,z)      ! take maximum between |x^2|,|a3 * x|

      deflateCase = 1        ! up to now, the maximum is |x^4| or |a3 * x^3|
      IF (y < b) THEN        ! check maximum between |x^2|,|a3 * x|,|a2|
          y = b * z          ! the maximum is |a2| -> form |a2 * x|
          deflateCase = 2    ! up to now, the maximum is |a2 * x^2|
      ELSE
          y = y * z          ! the maximum is |x^3| or |a3 * x^2|
      END IF
      IF (y < c) THEN        ! check maximum between |x^3|,|a3 * x^2|,|a2 * x|,|a1|
          y = c * z          ! the maximum is |a1| -> form |a1 * x|
          deflateCase = 3    ! up to now, the maximum is |a1 * x|
      ELSE
          y = y * z          ! the maximum is |x^4|,|a3 * x^3| or |a2 * x^2|
      END IF
      IF (y < d) THEN        ! check maximum between |x^4|,|a3 * x^3|,|a2 * x^2|,|a1 * x|,|a0|
          deflateCase = 4    ! the maximum is |a0|
      END IF

      x = x * k              ! 1st real root of original Q(x)
      SELECT CASE (deflateCase)
        CASE (1)
          z = 1.0 / x
          u = - q0 * z         ! u -> backward deflation on original Q(x)
          t = (u - q1) * z     ! t -> backward deflation on original Q(x)
          s = (t - q2) * z     ! s -> backward deflation on original Q(x)
        CASE (2)
          z = 1.0 / x
          u = - q0 * z         ! u -> backward deflation on original Q(x)
          t = (u - q1) * z     ! t -> backward deflation on original Q(x)
          s = q3 + x           ! s ->  forward deflation on original Q(x)
        CASE (3)
          s = q3 + x           ! s ->  forward deflation on original Q(x)
          t = q2 + s * x       ! t ->  forward deflation on original Q(x)
          u = - q0 / x         ! u -> backward deflation on original Q(x)
        CASE (4)
          s = q3 + x           ! s ->  forward deflation on original Q(x)
          t = q2 + s * x       ! t ->  forward deflation on original Q(x)
          u = q1 + t * x       ! u ->  forward deflation on original Q(x)
      END SELECT

      CALL CubicRoots (s, t, u, nReal, root (1:3,1:2))
      IF (nReal == 3) THEN
          s = root (1,Re)    !
          t = root (2,Re)    ! real roots of cubic are ordered s >= t >= u
          u = root (3,Re)    !

          root (1,Re) = MAX(s, x)
          root (2,Re) = MAX(t, MIN(s, x))
          root (3,Re) = MAX(u, MIN(t, x))
          root (4,Re) = MIN(u, x)
          root (:,Im) = 0.0
          nReal = 4
      ELSE                   ! there is only one real cubic root here
          s = root (1,Re)
          root (4,Re) = root (3,Re)
          root (3,Re) = root (2,Re)
          root (2,Re) = MIN(s, x)
          root (1,Re) = MAX(s, x)
          root (4,Im) = root (3,Im)
          root (3,Im) = root (2,Im)
          root (2,Im) = 0.0
          root (1,Im) = 0.0
          nReal = 2
      END IF
    ELSE
      ! If no real roots have been found by now, only complex roots are possible.
      ! Find real parts of roots first, followed by imaginary components.
      s = a3 * 0.5
      t =  s * s - a2
      u =  s * t + a1                   ! value of Q'(-a3/4) at stationary point -a3/4

      notZero = (ABS(u) >= macheps)    ! H(-a3/4) is considered > 0 at stationary point
      IF (a3 /= 0.0) THEN
          s = a1 / a3
          minimum = (a0 > s * s)                            ! H''(-a3/4) > 0 -> minimum
      ELSE
          minimum = (4 * a0 > a2 * a2)                      ! H''(-a3/4) > 0 -> minimum
      END IF

      iterate = notZero .or. (.not.notZero .and. minimum)
      IF (iterate) THEN
        x = SIGN(2.0,a3)                              ! initial root -> target = smaller mag root

        oscillate = 0
        bisection = .false.
        converged = .false.
        DO WHILE (.not.converged .and. .not.bisection)    ! Newton-Raphson iterates
          a = x + a3                                     !
          b = x + a                                      ! a = Q(x)
          c = x + b                                      !
          d = x + c                                      ! b = Q'(x)
          a = a * x + a2                                 !
          b = b * x + a                                  ! c = Q''(x) / 2
          c = c * x + b                                  !
          a = a * x + a1                                 ! d = Q'''(x) / 6
          b = b * x + a                                  !
          a = a * x + a0                                 !
          y = a * d * d - b * c * d + b * b              ! y = H(x), usually < 0
          z = 2 * d * (4 * a - b * d - c * c)            ! z = H'(x)
          IF (y > 0.0) THEN                           ! does Newton start oscillating ?
            oscillate = oscillate + 1                  ! increment oscillation counter
            s = x                                      ! save upper bisection bound
          ELSE
            u = x                                      ! save lower bisection bound
          END IF
          y = y / z                                      ! Newton correction
          x = x - y                                      ! new Newton root

          bisection = oscillate > 2                      ! activate bisection
          converged = ABS(y) <= ABS(x) * macheps       ! Newton convergence criterion
        END DO

        IF (bisection) THEN
          t = u - s                                     ! initial bisection interval
          DO WHILE (ABS(t) > ABS(x * macheps))        ! bisection iterates
            a = x + a3                                 !
            b = x + a                                  ! a = Q(x)
            c = x + b                                  !
            d = x + c                                  ! b = Q'(x)
            a = a * x + a2                             !
            b = b * x + a                              ! c = Q''(x) / 2
            c = c * x + b                              !
            a = a * x + a1                             ! d = Q'''(x) / 6
            b = b * x + a                              !
            a = a * x + a0                             !
            y = a * d * d - b * c * d + b * b          ! y = H(x)
            IF (y > 0.0) THEN                       !
                s = x                                  !
            ELSE                                       ! KEEP BRACKET ON ROOT
                u = x                                  !
            END IF                                     !
            t = 0.5 * (u - s)                       ! new bisection interval
            x = s + t                                  ! new bisection root
          END DO
        END IF

        a = x * k                                         ! 1st real component -> a
        b = - 0.5 * q3 - a                             ! 2nd real component -> b

        x = 4 * a + q3                                    ! Q'''(a)
        y = x + q3 + q3                                   !
        y = y * a + q2 + q2                               ! Q'(a)
        y = y * a + q1                                    !
        y = MAX(y / x, 0.0)                           ! ensure Q'(a) / Q'''(a) >= 0
        x = 4 * b + q3                                    ! Q'''(b)
        z = x + q3 + q3                                   !
        z = z * b + q2 + q2                               ! Q'(b)
        z = z * b + q1                                    !
        z = MAX(z / x, 0.0)                           ! ensure Q'(b) / Q'''(b) >= 0
        c = a * a                                         ! store a^2 for later
        d = b * b                                         ! store b^2 for later
        s = c + y                                         ! magnitude^2 of (a + iy) root
        t = d + z                                         ! magnitude^2 of (b + iz) root
        IF (s > t) THEN                                   ! minimize imaginary error
          c = SQRT(y)                                  ! 1st imaginary component -> c
          d = SQRT(q0 / s - d)                         ! 2nd imaginary component -> d
        ELSE
          c = SQRT(q0 / t - c)                         ! 1st imaginary component -> c
          d = SQRT(z)                                  ! 2nd imaginary component -> d
        END IF
      ELSE                                                  ! no bisection -> real components equal
        a = - 0.25 * q3                                ! 1st real component -> a
        b = a                                             ! 2nd real component -> b = a
        x = a + q3                                        !
        x = x * a + q2                                    ! Q(a)
        x = x * a + q1                                    !
        x = x * a + q0                                    !
        y = - 0.1875 * q3 * q3 + 0.5 * q2           ! Q''(a) / 2
        z = MAX(y * y - x, 0.0)                       ! force discriminant to be >= 0
        z = SQRT(z)                                      ! square root of discriminant
        y = y + SIGN(z,y)                                ! larger magnitude root
        x = x / y                                         ! smaller magnitude root
        c = MAX(y, 0.0)                               ! ensure root of biquadratic > 0
        d = MAX(x, 0.0)                               ! ensure root of biquadratic > 0
        c = SQRT(c)                                      ! large magnitude imaginary component
        d = SQRT(d)                                      ! small magnitude imaginary component
      END IF
      IF (a > b) THEN
        root (1,Re) = a
        root (2,Re) = a
        root (3,Re) = b
        root (4,Re) = b
        root (1,Im) = c
        root (2,Im) = - c
        root (3,Im) = d
        root (4,Im) = - d
      ELSE IF (a < b) THEN
        root (1,Re) = b
        root (2,Re) = b
        root (3,Re) = a
        root (4,Re) = a
        root (1,Im) = d
        root (2,Im) = - d
        root (3,Im) = c
        root (4,Im) = - c
      ELSE
        root (1,Re) = a
        root (2,Re) = a
        root (3,Re) = a
        root (4,Re) = a
        root (1,Im) = c
        root (2,Im) = - c
        root (3,Im) = d
        root (4,Im) = - d
      END IF
    END IF    ! # of real roots 'if'
END SELECT    ! quartic type select

! Ready!
RETURN

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE QuarticRoots
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE CubicRoots(c2,c1,c0,nReal,root) 
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)     :: c2, c1, c0
INTEGER,INTENT(OUT) :: nReal
REAL,INTENT(OUT)    :: root (1:3,1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL           :: bisection
LOGICAL           :: converged

INTEGER           :: cubicType
INTEGER           :: deflateCase
INTEGER           :: oscillate

INTEGER,PARAMETER :: Re = 1
INTEGER,PARAMETER :: Im = 2

INTEGER,PARAMETER :: allzero   = 0
INTEGER,PARAMETER :: linear    = 1
INTEGER,PARAMETER :: quadratic = 2
INTEGER,PARAMETER :: general   = 3
REAL              :: a0, a1, a2
REAL              :: a, b, c, k, s, t, u, x, y, z
REAL              :: xShift
REAL,PARAMETER    :: macheps = epsilon (1.0)
REAL,PARAMETER    :: one27th = 1.0 / 27.0
REAL,PARAMETER    :: two27th = 2.0 / 27.0
REAL,PARAMETER    :: third   = 1.0 /  3.0
REAL,PARAMETER    :: p1 = 1.09574         !
REAL,PARAMETER    :: q1 = 3.23900e-1      ! Newton-Raphson coeffs for class 1 and 2
REAL,PARAMETER    :: r1 = 3.23900e-1      !
REAL,PARAMETER    :: s1 = 9.57439e-2      !
REAL,PARAMETER    :: p3 = 1.14413         !
REAL,PARAMETER    :: q3 = 2.75509e-1      ! Newton-Raphson coeffs for class 3
REAL,PARAMETER    :: r3 = 4.45578e-1      !
REAL,PARAMETER    :: s3 = 2.59342e-2      !
REAL,PARAMETER    :: q4 = 7.71845e-1      ! Newton-Raphson coeffs for class 4
REAL,PARAMETER    :: s4 = 2.28155e-1      !
REAL,PARAMETER    :: p51 = 8.78558e-1     !
REAL,PARAMETER    :: p52 = 1.92823e-1     !
REAL,PARAMETER    :: p53 = 1.19748        !
REAL,PARAMETER    :: p54 = 3.45219e-1     !
REAL,PARAMETER    :: q51 = 5.71888e-1     !
REAL,PARAMETER    :: q52 = 5.66324e-1     !
REAL,PARAMETER    :: q53 = 2.83772e-1     ! Newton-Raphson coeffs for class 5 and 6
REAL,PARAMETER    :: q54 = 4.01231e-1     !
REAL,PARAMETER    :: r51 = 7.11154e-1     !
REAL,PARAMETER    :: r52 = 5.05734e-1     !
REAL,PARAMETER    :: r53 = 8.37476e-1     !
REAL,PARAMETER    :: r54 = 2.07216e-1     !
REAL,PARAMETER    :: s51 = 3.22313e-1     !
REAL,PARAMETER    :: s52 = 2.64881e-1     !
REAL,PARAMETER    :: s53 = 3.56228e-1     !
REAL,PARAMETER    :: s54 = 4.45532e-3     !
!----------------------------------------------------------------------------------------------------------------------!

! Start
! Handle special cases.
! 1) all terms zero
! 2) only quadratic term is nonzero -> linear equation.
! 3) only independent term is zero -> quadratic equation.
IF (c0 == 0.0 .and. c1 == 0.0 .and. c2 == 0.0) THEN
  cubicType = allzero
ELSE IF (c0 == 0.0 .and. c1 == 0.0) THEN
  k  = 1.0
  a2 = c2
  cubicType = linear
ELSE IF (c0 == 0.0) THEN
  k  = 1.0
  a2 = c2
  a1 = c1
  cubicType = quadratic
ELSE
  ! The general case. Rescale cubic polynomial, such that largest absolute coefficient
  ! is (exactly!) equal to 1. Honor the presence of a special cubic case that might have
  ! been obtained during the rescaling process (due to underflow in the coefficients).
  x = ABS(c2)
  y = SQRT(ABS(c1))
  z = ABS(c0) ** third
  u = MAX(x,y,z)

  IF (u == x) THEN
    k  = 1.0 / x
    a2 = SIGN(1.0 , c2)
    a1 = (c1 * k) * k
    a0 = ((c0 * k) * k) * k
  ELSE IF (u == y) THEN
    k  = 1.0 / y
    a2 = c2 * k
    a1 = SIGN(1.0 , c1)
    a0 = ((c0 * k) * k) * k
  ELSE
    k  = 1.0 / z
    a2 = c2 * k
    a1 = (c1 * k) * k
    a0 = SIGN(1.0 , c0)
  END IF
  
  k = 1.0 / k

  IF (a0 == 0.0 .and. a1 == 0.0 .and. a2 == 0.0) THEN
    cubicType = allzero
  ELSE IF (a0 == 0.0 .and. a1 == 0.0) THEN
    cubicType = linear
  ELSE IF (a0 == 0.0) THEN
    cubicType = quadratic
  ELSE
    cubicType = general
  END IF
END IF

! Select the case.
SELECT CASE (cubicType)
  ! 1) Only zero roots.
  CASE (allzero)
    nReal = 3

    root (:,Re) = 0.0
    root (:,Im) = 0.0

  ! 2) The linear equation case -> additional 2 zeros.
  CASE (linear)
    x = - a2 * k

    nReal = 3
    root (1,Re) = MAX(0.0, x)
    root (2,Re) = 0.0
    root (3,Re) = MIN(0.0, x)
    root (:,Im) = 0.0
  ! 3) The quadratic equation case -> additional 1 zero.
  CASE (quadratic)
    CALL quadraticRoots (a2, a1, nReal, root (1:2,1:2))
    IF (nReal == 2) THEN
      x = root (1,1) * k         ! real roots of quadratic are ordered x >= y
      y = root (2,1) * k

      nReal = 3
      root (1,Re) = MAX(x, 0.0)
      root (2,Re) = MAX(y, MIN(x, 0.0))
      root (3,Re) = MIN(y, 0.0)
      root (:,Im) = 0.0
    ELSE
      nReal = 1
      root (3,Re) = root (2,Re) * k
      root (2,Re) = root (1,Re) * k
      root (1,Re) = 0.0
      root (3,Im) = root (2,Im) * k
      root (2,Im) = root (1,Im) * k
      root (1,Im) = 0.0
    END IF
  ! 3) The general cubic case. Set the best Newton-Raphson root estimates for the cubic.
  !    The easiest and most robust conditions are checked first. The most complicated
  !    ones are last and only done when absolutely necessary.
  CASE (general)
    IF (a0 == 1.0) THEN
      x = - p1 + q1 * a1 - a2 * (r1 - s1 * a1)

      a = a2
      b = a1
      c = a0
      xShift = 0.0
    ELSE IF (a0 == - 1.0) THEN
      x = p1 - q1 * a1 - a2 * (r1 - s1 * a1)

      a = a2
      b = a1
      c = a0
      xShift = 0.0
    ELSE IF (a1 == 1.0) THEN
      IF (a0 > 0.0) THEN
          x = a0 * (- q4 - s4 * a2)
      ELSE
          x = a0 * (- q4 + s4 * a2)
      END IF
      a = a2
      b = a1
      c = a0
      xShift = 0.0
    ELSE IF (a1 == - 1.0) THEN
      y = - two27th
      y = y * a2
      y = y * a2 - third
      y = y * a2

      IF (a0 < y) THEN
          x = + p3 - q3 * a0 - a2 * (r3 + s3 * a0)               ! + guess
      ELSE
          x = - p3 - q3 * a0 - a2 * (r3 - s3 * a0)               ! - guess
      END IF
      a = a2
      b = a1
      c = a0
      xShift = 0.0
    ELSE IF (a2 == 1.0) THEN
      b = a1 - third
      c = a0 - one27th
    IF (ABS(b) < macheps .and. ABS(c) < macheps) THEN        ! triple -1/3 root
      x = - third * k

      nReal = 3
      root (:,Re) = x
      root (:,Im) = 0.0
      return
    ELSE
      y = third * a1 - two27th

      IF (a1 <= third) THEN
        IF (a0 > y) THEN
          x = - p51 - q51 * a0 + a1 * (r51 - s51 * a0)   ! - guess
        ELSE
          x = + p52 - q52 * a0 - a1 * (r52 + s52 * a0)   ! + guess
        END IF
      ELSE
        IF (a0 > y) THEN
          x = - p53 - q53 * a0 + a1 * (r53 - s53 * a0)   ! <-1/3 guess
        ELSE
          x = + p54 - q54 * a0 - a1 * (r54 + s54 * a0)   ! >-1/3 guess
        END IF
      END IF

      IF (ABS(b) < 1.e-2 .and. ABS(c) < 1.e-2) THEN  ! use shifted root
        c = - third * b + c
        IF (ABS(c) < macheps) c = 0.0                  ! prevent random noise
          a = 0.0
          xShift = third
          x = x + xShift
        ELSE
          a = a2
          b = a1
          c = a0
          xShift = 0.0
        END IF
      END IF
    ELSE IF (a2 == - 1.0) THEN
      b = a1 - third
      c = a0 + one27th
      IF (ABS(b) < macheps .and. ABS(c) < macheps) THEN        ! triple 1/3 root
        x = third * k
        nReal = 3
        root (:,Re) = x
        root (:,Im) = 0.0
        return
      ELSE
        y = two27th - third * a1
        IF (a1 <= third) THEN
          IF (a0 < y) THEN
            x = + p51 - q51 * a0 - a1 * (r51 + s51 * a0)   ! +1 guess
          ELSE
            x = - p52 - q52 * a0 + a1 * (r52 - s52 * a0)   ! -1 guess
          END IF
        ELSE
          IF (a0 < y) THEN
            x = + p53 - q53 * a0 - a1 * (r53 + s53 * a0)   ! >1/3 guess
          ELSE
            x = - p54 - q54 * a0 + a1 * (r54 - s54 * a0)   ! <1/3 guess
          END IF
        END IF
        IF (ABS(b) < 1.e-2 .and. ABS(c) < 1.e-2) THEN  ! use shifted root
          c = third * b + c
          IF (ABS(c) < macheps) c = 0.0                  ! prevent random noise
            a = 0.0
            xShift = - third
            x = x + xShift
          ELSE
            a = a2
            b = a1
            c = a0
            xShift = 0.0
          END IF
        END IF
      END IF
      ! Perform Newton/Bisection iterations on x^3 + ax^2 + bx + c.
      z = x + a
      y = x + z
      z = z * x + b
      y = y * x + z       ! C'(x)
      z = z * x + c       ! C(x)
      t = z               ! save C(x) for sign comparison
      x = x - z / y       ! 1st improved root

      oscillate = 0
      bisection = .false.
      converged = .false.

      DO WHILE (.not.converged .and. .not.bisection)    ! Newton-Raphson iterates
        z = x + a
        y = x + z
        z = z * x + b
        y = y * x + z
        z = z * x + c
        IF (z * t < 0.0) THEN                       ! does Newton start oscillating ?
          IF (z < 0.0) THEN
            oscillate = oscillate + 1              ! increment oscillation counter
            s = x                                  ! save lower bisection bound
          ELSE
            u = x                                  ! save upper bisection bound
          END IF
          t = z                                      ! save current C(x)
        END IF
        y = z / y                                      ! Newton correction
        x = x - y                                      ! new Newton root

        bisection = oscillate > 2                      ! activate bisection
        converged = ABS(y) <= ABS(x) * macheps       ! Newton convergence indicator
      END DO

      IF (bisection) THEN
        t = u - s                                     ! initial bisection interval
        DO WHILE(ABS(t) > ABS(x) * macheps)        ! bisection iterates
          z = x + a                                  !
          z = z * x + b                              ! C (x)
          z = z * x + c                              !
          IF (z < 0.0) THEN                       !
            s = x                                  !
          ELSE                                       ! keep bracket on root
            u = x                                  !
          END IF                                     !
          t = 0.5 * (u - s)                       ! new bisection interval
          x = s + t                                  ! new bisection root
        END DO
      END IF
      x = x - xShift                                    ! unshift root
      ! Forward / backward deflate rescaled cubic (if needed) to check for other real roots.
      ! The deflation analysis is performed on the rescaled cubic. The actual deflation must
      ! be performed on the original cubic, not the rescaled one. Otherwise deflation errors
      ! will be enhanced when undoing the rescaling on the extra roots.
      z = ABS(x)
      s = ABS(a2)
      t = ABS(a1)
      u = ABS(a0)
      y = z * MAX(s,z)           ! take maximum between |x^2|,|a2 * x|
      deflateCase = 1             ! up to now, the maximum is |x^3| or |a2 * x^2|
      IF (y < t) THEN             ! check maximum between |x^2|,|a2 * x|,|a1|
        y = t * z               ! the maximum is |a1 * x|
        deflateCase = 2         ! up to now, the maximum is |a1 * x|
      ELSE
        y = y * z               ! the maximum is |x^3| or |a2 * x^2|
      END IF
      IF (y < u) THEN             ! check maximum between |x^3|,|a2 * x^2|,|a1 * x|,|a0|
        deflateCase = 3         ! the maximum is |a0|
      END IF
      y = x * k                   ! real root of original cubic
      SELECT CASE (deflateCase)
        CASE (1)
          x = 1.0 / y
          t = - c0 * x              ! t -> backward deflation on unscaled cubic
          s = (t - c1) * x          ! s -> backward deflation on unscaled cubic
        CASE (2)
          s = c2 + y                ! s ->  forward deflation on unscaled cubic
          t = - c0 / y              ! t -> backward deflation on unscaled cubic
        CASE (3)
          s = c2 + y                ! s ->  forward deflation on unscaled cubic
          t = c1 + s * y            ! t ->  forward deflation on unscaled cubic
      END SELECT

      CALL QuadraticRoots(s,t,nReal,root(1:2,1:2))
      IF (nReal == 2) THEN
        x = root (1,Re)         ! real roots of quadratic are ordered x >= z
        z = root (2,Re)         ! use 'z', because 'y' is original cubic real root

        nReal = 3
        root (1,Re) = MAX(x, y)
        root (2,Re) = MAX(z, MIN(x, y))
        root (3,Re) = MIN(z, y)
        root (:,Im) = 0.0
      ELSE
        nReal = 1
        root (3,Re) = root (2,Re)
        root (2,Re) = root (1,Re)
        root (1,Re) = y
        root (3,Im) = root (2,Im)
        root (2,Im) = root (1,Im)
        root (1,Im) = 0.0
      END IF
END SELECT

! Ready!
RETURN

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE CubicRoots
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE QuadraticRoots(q1,q0,nReal,root)
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
REAL,INTENT(IN)     :: q1
REAL,INTENT(IN)     :: q0
INTEGER,INTENT(OUT) :: nReal
REAL,INTENT(OUT)    :: root(1:2,1:2)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
LOGICAL           :: rescale
REAL              :: a0, a1
REAL              :: k, x, y, z
REAL,PARAMETER    :: LPN     = HUGE(1.0)   ! the (L)argest (P)ositive (N)umber
REAL,PARAMETER    :: sqrtLPN = SQRT(LPN)      ! and the square root of it
INTEGER,PARAMETER :: Re = 1
INTEGER,PARAMETER :: Im = 2
!----------------------------------------------------------------------------------------------------------------------!

! Handle special cases
IF (q0 == 0.0 .and. q1 == 0.0) THEN
  nReal = 2
  root (:,Re) = 0.0
  root (:,Im) = 0.0
ELSE IF (q0 == 0.0) THEN
  nReal = 2
  root (1,Re) = MAX(0.0, - q1)
  root (2,Re) = MIN(0.0, - q1)
  root (:,Im) = 0.0
ELSE IF (q1 == 0.0) THEN
  x = SQRT(ABS(q0))
  IF (q0 < 0.0) THEN
    nReal = 2
    root (1,Re) = x
    root (2,Re) = - x
    root (:,Im) = 0.0
  ELSE
    nReal = 0
    root (:,Re) = 0.0
    root (1,Im) = x
    root (2,Im) = - x
  END IF
ELSE
  ! The general case. Do rescaling, if either squaring of q1/2 or evaluation of
  ! (q1/2)^2 - q0 will lead to overflow. This is better than to have the solver
  ! crashed. Note, that rescaling might lead to loss of accuracy, so we only
  ! invoke it when absolutely necessary.
  rescale = (q1 > sqrtLPN + sqrtLPN)     ! this detects overflow of (q1/2)^2
  IF (.not.rescale) THEN
    x = q1 * 0.5                      ! we are sure here that x*x will not overflow
    rescale = (q0 < x * x - LPN)      ! this detects overflow of (q1/2)^2 - q0
  END IF
  IF (rescale) THEN
    x = ABS(q1)
    y = SQRT(ABS(q0))
    IF (x > y) THEN
      k  = x
      z  = 1.0 / x
      a1 = SIGN(1.0 , q1)
      a0 = (q0 * z) * z
    ELSE
      k  = y
      a1 = q1 / y
      a0 = SIGN(1.0 , q0)
    END IF
  ELSE
    a1 = q1
    a0 = q0
  END IF
  ! Determine the roots of the quadratic. Note, that either a1 or a0 might
  ! have become equal to zero due to underflow. But both cannot be zero.
  x = a1 * 0.5
  y = x * x - a0
  IF (y >= 0.0) THEN
    y = SQRT(y)
    IF (x > 0.0) THEN
      y = - x - y
    ELSE
      y = - x + y
    END IF
    IF (rescale) THEN
      y = y * k                     ! very important to convert to original
      z = q0 / y                    ! root first, otherwise complete loss of
    ELSE                            ! root due to possible a0 = 0 underflow
      z = a0 / y
    END IF
    nReal = 2
    root (1,Re) = MAX(y,z)           ! 1st real root of x^2 + a1 * x + a0
    root (2,Re) = MIN(y,z)           ! 2nd real root of x^2 + a1 * x + a0
    root (:,Im) = 0.0
  ELSE
    y = SQRT(- y)
    nReal = 0
    root (1,Re) = - x
    root (2,Re) = - x
    root (1,Im) = y                   ! complex conjugate pair of roots
    root (2,Im) = - y                 ! of x^2 + a1 * x + a0
    IF (rescale) THEN
      root = root * k
    END IF
  END IF
END IF

! Ready!
RETURN

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE QuadraticRoots
!======================================================================================================================!
!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_PolynomialRoots
!======================================================================================================================!
