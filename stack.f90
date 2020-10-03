program stack
   use constants
   use aiff
   use io
   implicit none

   integer :: i, n, t
   real(dp), allocatable :: sound(:, :)
   type(audio), allocatable :: p(:)
   type(audio) :: s

   s%channels = 2
   s%points = 0

   n = command_argument_count()

   allocate(p(n - 1))

   do i = 1, n - 1
      call take(command_argument(i), p(i))

      if (p(i)%channels .ne. 2) then
         write (*, "('ERROR: only two channels supported')")
         stop
      end if

      s%points = max(s%points, p(i)%points)
   end do

   s%rate = p(n - 1)%rate

   allocate(s%sound(s%channels, s%points))
   allocate(  sound(s%channels, s%points))

   sound(:, :) = 0.0_dp

   do i = 1, n - 1
      do t = 1, s%points
        sound(:, t) = sound(:, t) &
          + p(i)%amplitude * p(i)%sound(:, 1 + modulo(t, p(i)%points))
      end do
   end do

   sound(:, :) = sound / i2max

   s%amplitude = maxval(abs(sound))
   s%sound = nint(i2max / s%amplitude * sound, i2)

   call make(command_argument(n), s)
end program stack
