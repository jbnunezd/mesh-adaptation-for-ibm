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
MODULE MOD_MeshBuiltIn
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_GLOBAL_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
PRIVATE
!----------------------------------------------------------------------------------------------------------------------!
INTERFACE InitializeMeshBuiltIn
  MODULE PROCEDURE InitializeMeshBuiltIn
END INTERFACE

INTERFACE MeshBuiltIn
  MODULE PROCEDURE MeshBuiltIn
END INTERFACE
!----------------------------------------------------------------------------------------------------------------------!
PUBLIC :: InitializeMeshBuiltIn
PUBLIC :: MeshBuiltIn
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
SUBROUTINE InitializeMeshBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: Header
!----------------------------------------------------------------------------------------------------------------------!

IF (InitializeMeshBuiltInIsDone) THEN
  SWRITE(UNIT_SCREEN,*) "InitializeMeshBuiltIn not ready to be called or already called."
  RETURN
END IF

Header = "INITIALIZING BUILT-IN MESH MODULE..."
CALL PrintHeader(Header)

PP_NGeo = 1

CALL InitializeMeshBasis()
CALL InitializeMeshBuiltIn3D()

InitializeMeshBuiltInIsDone = .TRUE.

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBuiltIn
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeMeshBasis()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
!----------------------------------------------------------------------------------------------------------------------!

PP_N = PP_NGeo

! Constructing xNodes, xWeights, xBaryWeights, and DMatrix

! Chebyshev-Gauss-Lobatto (PP_NGeo)
ALLOCATE(CGL_xNodes_NGeo(0:PP_NGeo))
ALLOCATE(CGL_xWeights_NGeo(0:PP_NGeo))
ALLOCATE(CGL_xBaryWeights_NGeo(0:PP_NGeo))
ALLOCATE(CGL_DMatrix_NGeo(0:PP_NGeo,0:PP_NGeo))
CALL ChebyshevGaussLobattoNodesAndWeights(PP_NGeo,CGL_xNodes_NGeo,CGL_xWeights_NGeo)
CALL BarycentricWeights(PP_NGeo,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo)
CALL PolynomialDerivativeMatrix(PP_NGeo,CGL_xNodes_NGeo,CGL_DMatrix_NGeo)

! Vandermonde Matrices
! Chebyshev-Gauss-Lobatto (PP_NGeo) to Equidistant (PP_N)
ALLOCATE(UNIFORM_xNodes_N(0:PP_N))
DO i=0,PP_N
  UNIFORM_xNodes_N(i) = -1.0+(2.0/REAL(PP_N))*REAL(i)
END DO
ALLOCATE(VDM_CGLNGeo_UniformN(0:PP_N,0:PP_NGeo))
CALL PolynomialInterpolationMatrix(&
  PP_NGeo,PP_N,CGL_xNodes_NGeo,CGL_xBaryWeights_NGeo,UNIFORM_xNodes_N,VDM_CGLNGeo_UniformN)

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBasis
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE InitializeMeshBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_DataStructures
USE MOD_ConfigFilesTools
USE MOD_NumericsTools
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nElems
USE MOD_MeshMain_vars,ONLY: PP_nNodes
USE MOD_MeshMain_vars,ONLY: MeshData_BCIndex
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryName
USE MOD_MeshMain_vars,ONLY: MeshData_BoundaryType
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltInMappings3D,ONLY: Mapping3D_StraightHexa
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltInStretchingFunctions3D,ONLY: MeshStretching3D_Uniform
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i
INTEGER :: nBoxCorners
INTEGER :: nBoundaries
!----------------------------------------------------------------------------------------------------------------------!
CHARACTER(LEN=256) :: ErrorMessage
!----------------------------------------------------------------------------------------------------------------------!

ParametersMeshBuiltIn3D%nBoxElems3D = GetIntegerArray('nBoxElems3D',3,'10,10,10')

nBoxCorners = CountStrings('BoxCorner',0)

SELECT CASE(nBoxCorners)
  CASE(8)
    DO i=1,nBoxCorners
      ParametersMeshBuiltIn3D%BoxCorner3D(i,:) = GetRealArray('BoxCorner',3)
    END DO
  CASE DEFAULT
    ErrorMessage = "Wrong number of BoxCorners"
    CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

ParametersMeshBuiltIn3D%WhichMeshType       = GetString('WhichMeshType','cartesian-domain')
ParametersMeshBuiltIn3D%WhichMapping3D      = GetString('WhichMapping3D','straight-hexa')
ParametersMeshBuiltIn3D%WhichOutputBasis    = GetString('WhichOutputBasis','uniform')
ParametersMeshBuiltIn3D%StretchMesh         = GetLogical('StretchMesh','.FALSE.')
ParametersMeshBuiltIn3D%WhichMeshStretching = GetString('WhichMeshStretching','uniform')
ParametersMeshBuiltIn3D%DebugMeshBuiltIn    = GetLogical('DebugMeshBuiltIn','.FALSE.')

! Boundary Conditions
ParametersMeshBuiltIn3D%BCIndex = GetIntegerArray('BCIndex',6)

nBoundaries = CountStrings('BoundaryType',0)
IF ((nBoundaries .EQ. 6) .OR. (nBoundaries .EQ. 7)) THEN
  ALLOCATE(ParametersMeshBuiltIn3D%BoundaryName(1:nBoundaries))
  ALLOCATE(ParametersMeshBuiltIn3D%BoundaryType(1:nBoundaries,1:3))
  DO i=1,6
    ParametersMeshBuiltIn3D%BoundaryName(i)     = GetString('BoundaryName')
    ParametersMeshBuiltIn3D%BoundaryType(i,1:3) = GetIntegerArray('BoundaryType',3)
  END DO
ELSE
  ErrorMessage = "Wrong number of BoundaryType and BoundaryName"
  CALL PrintError(__STAMP__,ErrorMessage)
END IF

ALLOCATE(MeshData_BCIndex(1:6))
ALLOCATE(MeshData_BoundaryName(1:nBoundaries))
ALLOCATE(MeshData_BoundaryType(1:nBoundaries,1:3))

MeshData_BCIndex      = ParametersMeshBuiltIn3D%BCIndex
MeshData_BoundaryName = ParametersMeshBuiltIn3D%BoundaryName
MeshData_BoundaryType = ParametersMeshBuiltIn3D%BoundaryType

SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichMeshType))
  CASE('cartesian-domain')
    BuildPhysicalDomain3D => BuildPhysicalDomain3D_CartesianDomain
  CASE DEFAULT
    ErrorMessage = "Mesh Type for BuildPhysicalDomain3D not implemented"
    CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichMapping3D))
  CASE('straight-hexa')
    Mapping3D => Mapping3D_StraightHexa
  CASE DEFAULT
    ErrorMessage = "Analytical Mapping not implemented"
    CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichMeshStretching))
  CASE('uniform')
    MeshStretching3D => MeshStretching3D_Uniform
  CASE DEFAULT
    ErrorMessage = "Mesh Type for BuildPhysicalDomain not implemented"
    CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

ALLOCATE(VDM_CGLNGeo_OutputN(0:PP_N,0:PP_NGeo))
SELECT CASE(LowerCase(ParametersMeshBuiltIn3D%WhichOutputBasis))
  CASE('uniform')
    VDM_CGLNGeo_OutputN = VDM_CGLNGeo_UniformN
  CASE DEFAULT
    ErrorMessage = "Mesh Output Basis not implemented"
    CALL PrintError(__STAMP__,ErrorMessage)
END SELECT

! Mesh Variables
nBoxElems3D     = ParametersMeshBuiltIn3D%nBoxElems3D
BoxCorner3D     = ParametersMeshBuiltIn3D%BoxCorner3D
PP_nElems       = nBoxElems3D(1)*nBoxElems3D(2)*nBoxElems3D(3)
PP_nNodes       = PP_nElems*(PP_NGeo+1)**3

ALLOCATE(CGL_NGeo_Domain3D(1:3,0:PP_NGeo,0:PP_NGeo,0:PP_NGeo,1:PP_nElems))
ALLOCATE(MeshData_ElementsToNodesCoordinates3D(1:3,0:PP_N,0:PP_N,0:PP_N,1:PP_nElems))

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE InitializeMeshBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshBuiltIn()
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL MeshBuiltIn3D()

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshBuiltIn
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE MeshBuiltIn3D()
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nDims
USE MOD_MeshMain_vars,ONLY: PP_nElems
USE MOD_MeshMain_vars,ONLY: PP_nNodes
USE MOD_MeshMain_vars,ONLY: MeshInfo
USE MOD_MeshMain_vars,ONLY: MeshData_ElementsToNodesCoordinates3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: BoxCorner3D
USE MOD_MeshBuiltIn_vars,ONLY: nBoxElems3D
USE MOD_MeshBuiltIn_vars,ONLY: CGL_xNodes_NGeo
USE MOD_MeshBuiltIn_vars,ONLY: CGL_NGeo_Domain3D
USE MOD_MeshBuiltIn_vars,ONLY: BuildPhysicalDomain3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshAdapter_vars,ONLY: ParametersMeshAdapter
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!

CALL BuildReferenceDomain3D(&
  Nin       = PP_NGeo,&
  nElems    = PP_nElems,&
  nBoxElems = nBoxElems3D,&
  XRefDom1D = CGL_xNodes_NGeo,&
  XRefDom3D = CGL_NGeo_Domain3D)

CALL BuildPhysicalDomain3D(&
  Nin        = PP_NGeo,&
  nElems     = PP_nElems,&
  nBoxElems  = nBoxElems3D,&
  BoxCorners = BoxCorner3D,&
  XRefDom3D  = CGL_NGeo_Domain3D,&
  XPhysDom3D = MeshData_ElementsToNodesCoordinates3D)

DEALLOCATE(CGL_NGeo_Domain3D)

! Mesh Info for exporting
MeshInfo%nDims           = PP_nDims
MeshInfo%NGeo            = PP_NGeo
MeshInfo%nElems          = PP_nElems
MeshInfo%nNodes          = PP_nNodes
MeshInfo%nOutVars        = 2
MeshInfo%MaxRefLevel     = 0
MeshInfo%DataNames3D(1)  = "Level"
MeshInfo%DataNames3D(2)  = "Flag"
MeshInfo%CoordNames3D(1) = "CoordinateX"
MeshInfo%CoordNames3D(2) = "CoordinateY"
MeshInfo%CoordNames3D(3) = "CoordinateZ"
MeshInfo%FileVersion     = TRIM(ParametersMeshAdapter%FileVersion)
MeshInfo%ProgramName     = TRIM(ParametersMeshAdapter%ProgramName)
MeshInfo%ProjectName     = TRIM(ParametersMeshAdapter%ProjectName)
MeshInfo%BaseFileName    = TRIM(ParametersMeshAdapter%ProjectName)//"_MESH"

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE MeshBuiltIn3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildReferenceDomain3D(Nin,nElems,nBoxElems,XRefDom1D,XRefDom3D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: MeshStretching3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:3)
REAL,INTENT(IN)    :: XRefDom1D(0:Nin)
REAL,INTENT(OUT)   :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
INTEGER :: iElem
REAL    :: x1(1:3)
REAL    :: dx(1:nBoxElems(1))
REAL    :: dy(1:nBoxElems(2))
REAL    :: dz(1:nBoxElems(3))
!----------------------------------------------------------------------------------------------------------------------!

CALL MeshStretching3D(nBoxElems,dx,dy,dz)

iElem = 0
x1(3) = -1.0
DO kk=1,nBoxElems(3)
  x1(2) = -1.0
  DO jj=1,nBoxElems(2)
    x1(1) = -1.0
    DO ii=1,nBoxElems(1)
      iElem = iElem + 1
      DO k=0,Nin
        DO j=0,Nin
          DO i=0,Nin
            XRefDom3D(1,i,j,k,iElem) = x1(1) + 0.5*(1.0+XRefDom1D(i))*dx(ii)
            XRefDom3D(2,i,j,k,iElem) = x1(2) + 0.5*(1.0+XRefDom1D(j))*dy(jj)
            XRefDom3D(3,i,j,k,iElem) = x1(3) + 0.5*(1.0+XRefDom1D(k))*dz(kk)
          END DO
        END DO
      END DO
      x1(1) = x1(1) + dx(ii)
    END DO
    x1(2) = x1(2) + dy(jj)
  END DO
  x1(3) = x1(3) + dz(kk)
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildReferenceDomain3D
!======================================================================================================================!
!
!
!
!======================================================================================================================!
SUBROUTINE BuildPhysicalDomain3D_CartesianDomain(Nin,nElems,nBoxElems,BoxCorners,XRefDom3D,XPhysDom3D)
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshMain_vars,ONLY: PP_N
USE MOD_MeshMain_vars,ONLY: PP_NGeo
USE MOD_MeshMain_vars,ONLY: PP_nDims
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_MeshBuiltIn_vars,ONLY: VDM_CGLNGeo_OutputN
USE MOD_MeshBuiltIn_vars,ONLY: Mapping3D
!----------------------------------------------------------------------------------------------------------------------!
USE MOD_NumericsTools,ONLY: InterpolateToNewPoints3D
!----------------------------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!----------------------------------------------------------------------------------------------------------------------!
INTEGER,INTENT(IN) :: Nin
INTEGER,INTENT(IN) :: nElems
INTEGER,INTENT(IN) :: nBoxElems(1:3)
REAL,INTENT(IN)    :: BoxCorners(1:8,1:3)
REAL,INTENT(IN)    :: XRefDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
REAL,INTENT(OUT)   :: XPhysDom3D(1:3,0:Nin,0:Nin,0:Nin,1:nElems)
!----------------------------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!----------------------------------------------------------------------------------------------------------------------!
INTEGER :: iElem
INTEGER :: i, j, k
INTEGER :: ii, jj, kk
REAL    :: Lx, Ly, Lz
REAL    :: r, s, t
REAL    :: xc(1:8,1:3)
!----------------------------------------------------------------------------------------------------------------------!

xc(1:8,1:3) = BoxCorners(1:8,1:3)

Lx = ABS(BoxCorners(2,1)-BoxCorners(1,1))
Ly = ABS(BoxCorners(4,2)-BoxCorners(1,2))
Lz = ABS(BoxCorners(5,3)-BoxCorners(1,3))

iElem = 0
DO kk=1,nBoxElems(3)
  DO jj=1,nBoxElems(2)
    DO ii=1,nBoxElems(1)
      iElem = iElem + 1
      DO k=0,Nin
        DO j=0,Nin
          DO i=0,Nin
            r = XRefDom3D(1,i,j,k,iElem)
            s = XRefDom3D(2,i,j,k,iElem)
            t = XRefDom3D(3,i,j,k,iElem)
            XPhysDom3D(1,i,j,k,iElem) = BoxCorners(1,1) + 0.5*(1.0+r)*Lx
            XPhysDom3D(2,i,j,k,iElem) = BoxCorners(1,2) + 0.5*(1.0+s)*Ly
            XPhysDom3D(3,i,j,k,iElem) = BoxCorners(1,3) + 0.5*(1.0+t)*Lz
          END DO
        END DO
      END DO
      CALL InterpolateToNewPoints3D(&
        PP_nDims,PP_NGeo,PP_N,VDM_CGLNGeo_OutputN,XPhysDom3D(:,:,:,:,iElem),XPhysDom3D(:,:,:,:,iElem))
    END DO
  END DO
END DO

!----------------------------------------------------------------------------------------------------------------------!
END SUBROUTINE BuildPhysicalDomain3D_CartesianDomain
!======================================================================================================================!
!
!
!----------------------------------------------------------------------------------------------------------------------!
END MODULE MOD_MeshBuiltIn
!======================================================================================================================!
