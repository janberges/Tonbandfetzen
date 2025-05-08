subroutine harmonics
   use constants, only: dp
   use io, only: command_argument
   use samples, only: sample
   use spectra, only: fourier
   implicit none

   integer :: n
   integer, parameter :: nmax = 99

   real(dp) :: wave(2 * nmax), amplitude, phase
   complex(dp) :: spectrum(nmax)

   call sample(wave, 'wave', command_argument(1, 'circular'))

   call fourier(wave, spectrum)

   write (*, "(/ 'f(t) = sum r[n] cos(n omega t - phi[n])' /)")
   write (*, '(A2, 2A15)') 'n', 'r[n]', 'phi[n]'

   do n = 1, size(spectrum)
      amplitude = 2.0_dp * abs(spectrum(n))

      if (amplitude .lt. 1e-10_dp) cycle

      phase = atan2(aimag(spectrum(n)), real(spectrum(n)))

      write (*, '(I2, 2F15.10)') n, amplitude, phase
   end do
end subroutine harmonics
