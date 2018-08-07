program spectra_1D_k
!use fftw3
implicit none
include 'fftw3.f'
!implicit none

INTEGER nk, j, k, i , plan1,plan2
INTEGER, parameter:: N_FFT = 512
REAL*8  FFT_DATA_X(N_FFT), FFT_DATA_KE(1:N_FFT), FFT_DATA_TKE(1:N_FFT),DELTA_X, PI, AVG(N_FFT/2+1),KX
complex*16 FFT_COMPLEX_KE(1:N_FFT/2+1), FFT_COMPLEX_TKE(1:N_FFT/2+1)


    OPEN (30,file='FFT_DATA.dat',form='formatted',status='old')

!    nk =  size(fft_u_prime_data_nearwall(:,1))
    PI = 4*ATAN(1.0D0)
    KX = 0.01

!    DO k = nk-N_FFT,nk-1
!     DO j = 1,NZ  
 

!!!!!!!!!!!!!!!!!!!! U FFT DATA !!!!!!!!!!!!!!!!!!!!

      do i=1,N_FFT  
 
       read(30,*) FFT_DATA_X(i),FFT_DATA_KE(i), FFT_DATA_TKE(i)
!        FFT_DATA_X(i) = i      
      end do

!      do i=1,N_FFT
!
!       FFT_DATA_KE(i) =  sin(KX*FFT_DATA_X(i))
!       FFT_DATA_TKE(i) =  cos(KX*FFT_DATA_X(i))
!
!      end do

 

     DELTA_X = FFT_DATA_X(N_FFT) - FFT_DATA_X(1)

     write(6,*) 'I am here'



     call dfftw_plan_dft_r2c_1d(plan1,N_FFT,FFT_DATA_KE,FFT_COMPLEX_KE,FFTW_FORWARD,FFTW_ESTIMATE)
     call dfftw_execute(plan1,FFT_DATA_KE,FFT_COMPLEX_KE)

     call dfftw_destroy_plan(plan1)



     open(501,file='KE_FOURIER.dat',form='formatted',status='unknown')

      do j = 1,N_FFT/2+1
   
           write(501,113)  2*pi*(j-1)/DELTA_X,  CDABS(FFT_COMPLEX_KE(J)), FFT_DATA_KE(J)
      end do


     call dfftw_plan_dft_r2c_1d(plan2,N_FFT,FFT_DATA_TKE,FFT_COMPLEX_TKE,FFTW_FORWARD,FFTW_ESTIMATE)
     call dfftw_execute(plan2,FFT_DATA_TKE,FFT_COMPLEX_TKE)

     call dfftw_destroy_plan(plan2)




     open(502,file='TKE_FOURIER.dat',form='formatted',status='unknown')

      do j = 1,N_FFT/2+1

           write(502,114)  2*pi*(j)/DELTA_X,  CDABS(FFT_COMPLEX_TKE(J))
      end do

        
113     format(3f17.9)
114     format(2f17.9) 
  
end program

!----*|--.---------.---------.---------.---------.---------.---------.-|-------|
!      SUBROUTINE FFT_PHYSICAL_TO_FOURIER_IC(W,CW,NXF,NYF,NKXF,NKYF)
!----*|--.---------.---------.---------.---------.---------.---------.-|-------|

!      use variables
!      use fftw3
!      include 'header'
!      integer NXF,NYF,NKXF,NKYF,I,J
!      double precision W(NX,NY)
!      double complex  CW(NKX+1,NKY+1)
!
!
!       call dfftw_plan_dft_r2c_2d(plan1,NX,NY,W,CW,FFTW_ESTIMATE)
!
!
!       call dfftw_execute_dft_r2c(plan1,W,CW)
!
!       call dfftw_destroy_plan(plan1)
!
!       return
!
!       end

