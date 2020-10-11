program stick
   use constants
   use riff
   use io
   implicit none

   integer :: i, n, offset
   type(audio), allocatable :: p(:)
   type(audio) :: s

   s%channels = 2
   s%points = 0
   s%amplitude = 0.0_dp

   n = command_argument_count()

   allocate(p(n - 1))

   do i = 1, n - 1
      call take(command_argument(i), p(i))

      if (p(i)%channels .ne. 2) then
         write (*, "('ERROR: only two channels supported')")
         stop
      end if

      s%points = s%points + p(i)%points

      s%amplitude = max(s%amplitude, p(i)%amplitude)
   end do

   s%rate = p(n - 1)%rate

   allocate(s%sound(s%channels, s%points))

   offset = 0

   do i = 1, n - 1
      if (p(i)%amplitude .ne. s%amplitude) then
         p(i)%sound = int(p(i)%sound * p(i)%amplitude / s%amplitude, i2)
      end if

      s%sound(:, offset + 1:offset + p(i)%points) = p(i)%sound

      offset = offset + p(i)%points
   end do

   call make(command_argument(n), s)
end program stick
