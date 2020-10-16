program repeat
   use constants
   use io
   use paths
   use riff
   implicit none

   integer :: i
   character(:), allocatable :: n
   type(audio) :: s1
   type(audio) :: s

   if (command_argument_count() .ne. 3) then
      write (*, "('Usage: repeat <count> <infile> <outfile>')")
      write (*, "('See ''man repeat'' for more information.')")
      stop
   end if

   n = command_argument(1)

   call read_riff(command_argument(2), s1)

   read (n, *) s%points

   s%channels  = s1%channels
   s%points    = s1%points * s%points
   s%rate      = s1%rate
   s%amplitude = s1%amplitude

   allocate(s%sound(s%channels, s%points))

   do i = 1, s%points, s1%points
      s%sound(:, i:i + s1%points) = s1%sound
   end do

   call write_riff(command_argument(3), s)
end program repeat
