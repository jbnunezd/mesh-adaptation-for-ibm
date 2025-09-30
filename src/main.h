!======================================================================================================================!
! MeshAdapter variables
!----------------------------------------------------------------------------------------------------------------------!
#ifndef __FILENAME__ 
#define __FILENAME__ __FILE__
#endif
!#define __STAMP__ __FILENAME__,__LINE__,__DATE__,__TIME__
#define __STAMP__ __FILENAME__,__LINE__,__DATE_ISO__,__TIME_ISO__

#ifdef GNU
#  define IEEE_IS_NAN ISNAN
#endif

#if MPI
#  define SWRITE IF (MPIROOT) WRITE
#else
#  define SWRITE WRITE
#endif
