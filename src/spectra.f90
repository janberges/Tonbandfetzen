module spectra
   use constants
   implicit none
   private

   public :: fourier

contains

   subroutine fourier(wave, spectrum)
      real(dp), intent(in) :: wave(:)
      complex(dp), intent(out) :: spectrum(:)

      integer :: n, m
      real(dp) :: omega, phi
      complex(dp) :: transform(size(spectrum), size(wave))

      omega = 2 * pi / size(wave)

      do m = 1, size(wave)
         do n = 1, size(spectrum)
            phi = n * m * omega
            transform(n, m) = cmplx(cos(phi), sin(phi), dp)
         end do
      end do

      spectrum = matmul(transform, wave) / size(wave)
   end subroutine fourier
end module spectra
