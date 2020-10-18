program harmonics
   use constants
   use io
   use samples
   use spectra
   implicit none

   integer :: n
   integer, parameter :: nmax = 100

   real(dp) :: wave(2 * nmax), amplitude, phase
   complex(dp) :: spectrum(nmax)

   if (command_argument_count() .ne. 1) then
      write (*, "('Usage: harmonics <label>')")
      write (*, "('See ''man harmonics'' for more information.')")
      stop
   end if

   call sample(wave, 'wave', command_argument(1))

   call fourier(wave, spectrum)

   write (*, "(/ 'f(t) = sum r[n] cos(n omega t - phi[n])' /)")
   write (*, '(A2, 2A15)') 'n', 'r[n]', 'phi[n]'

   do n = 1, size(spectrum)
      amplitude = 2 * abs(spectrum(n))

      if (amplitude .lt. 1e-10_dp) cycle

      phase = atan2(aimag(spectrum(n)), real(spectrum(n)))

      write (*, '(I2, 2F15.10)') n, amplitude, phase
   end do
end program harmonics
