program repeat
   use constants
   use io
   use rationals
   use riff
   implicit none

   integer :: i
   real(dp) :: factor
   type(audio) :: s1
   type(audio) :: s

   if (command_argument_count() .ne. 3) then
      write (*, "('Usage: repeat <count> <infile> <outfile>')")
      write (*, "('See ''man repeat'' for more information.')")
      stop
   end if

   factor = rational(command_argument(1))

   call read_riff(command_argument(2), s1)

   s%channels  = s1%channels
   s%points    = nint(abs(factor) * s1%points)
   s%rate      = s1%rate
   s%amplitude = s1%amplitude

   allocate(s%sound(s%channels, s%points))

   if (factor .gt. 0.0_dp) then
      do i = 1, s%points
         s%sound(:, i) = s1%sound(:, 1 + modulo(i - 1, s1%points))
      end do
   else
      do i = 1, s%points
         s%sound(:, s%points + 1 - i) &
            = s1%sound(:, 1 + modulo(s1%points - i, s1%points))
      end do
   end if

   call write_riff(command_argument(3), s)
end program repeat
