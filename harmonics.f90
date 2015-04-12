program harmonics
   use accuracy
   use io
   use samples
   use spectra
   implicit none

   integer :: i, n
   integer, parameter :: nmax = 100

   real(dp) :: wave(2 * nmax), amplitude
   complex(dp) :: spectrum(nmax)

   character(:), allocatable :: what

   do i = 1, command_argument_count()
      what = command_argument(i)

      call sample(wave, 'loop', what)

      call fourier(wave, spectrum)

      write (*, "(/'f(t) = sum_n a_n cos(n omega t - phi_n)'/)")
      write (*, '(A2, A15, A15)') 'n', 'a_n', 'phi_n'

      do n = 1, size(spectrum)
         amplitude = 2.0_dp * abs(spectrum(n))

         if (amplitude .lt. 1.0e-10_dp) cycle

         write (*, '(I2, F15.10, SP, F15.10)') n, amplitude, arg(spectrum(n))
      end do
   end do
end program harmonics
