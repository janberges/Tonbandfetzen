program stack
   use constants
   use aiff
   use io
   implicit none

   integer :: i, n
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
      sound(:, :p(i)%points) = sound(:, :p(i)%points) + p(i)%amplitude * p(i)%sound
   end do

   sound(:, :) = sound / (2 ** 15 - 1)

   s%amplitude = maxval(abs(sound))
   s%sound = nint((2 ** 15 - 1) / s%amplitude * sound, i2)

   call make(command_argument(n), s)
end program stack
