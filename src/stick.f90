program stick
   use constants
   use io
   use riff
   implicit none

   integer :: i, n, offset
   type(audio), allocatable :: p(:)
   type(audio) :: s

   s%channels  = 2
   s%points    = 0
   s%rate      = 1.0_dp
   s%amplitude = 0.0_dp

   n = command_argument_count()

   if (n .eq. 0) then
      write (*, "('Usage: stick [<infile> ...] <outfile>')")
      write (*, "('See ''man stick'' for more information.')")
      stop
   end if

   allocate(p(n - 1))

   do i = 1, n - 1
      call read_riff(command_argument(i), p(i))

      if (p(i)%channels .ne. 2) then
         write (*, "('ERROR: only two channels supported')")
         stop
      end if

      s%points = s%points + p(i)%points

      s%amplitude = max(s%amplitude, p(i)%amplitude)
      s%rate      = max(s%rate,      p(i)%rate)
   end do

   allocate(s%sound(s%channels, s%points))

   offset = 0

   do i = 1, n - 1
      if (p(i)%amplitude .ne. s%amplitude) then
         p(i)%sound = int(p(i)%sound * p(i)%amplitude / s%amplitude, i2)
      end if

      s%sound(:, offset + 1:offset + p(i)%points) = p(i)%sound

      offset = offset + p(i)%points
   end do

   call write_riff(command_argument(n), s)
end program stick
