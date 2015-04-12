module spectra
   use constants
   use intervals
   implicit none
   private

   public :: fourier, euler, arg

contains

   subroutine fourier(wave, spectrum)
      real(dp), intent(in) :: wave(:)
      complex(dp), intent(out) :: spectrum(:)

      integer :: t
      real(dp) :: omega(size(wave))
      complex(dp) :: transform(size(spectrum), size(wave))

      call interval(omega, 0.0_dp, 2.0_dp * pi, 1)

      do t = 1, size(wave)
         transform(:, t) = euler(omega(:size(spectrum)) * t)
      end do

      spectrum = matmul(transform, wave) / size(wave)
   end subroutine fourier

   elemental function euler(phase)
      complex(dp) :: euler

      real(dp), intent(in) :: phase

      euler = cmplx(cos(phase), sin(phase), dp)
   end function euler

   elemental function arg(z)
      real(dp) :: arg

      complex(dp), intent(in) :: z

      arg = atan2(aimag(z), real(z))
   end function arg
end module spectra
