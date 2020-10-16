program aiff2riff
   use aiff
   use constants
   use io
   use riff
   implicit none

   type(audio) :: s

   if (command_argument_count() .ne. 2) then
      write (*, "('Usage: aiff2riff <infile> <outfile>')")
      write (*, "('See ''man aiff2riff'' for more information.')")
      stop
   end if

   call read_aiff(command_argument(1), s)
   call write_riff(command_argument(2), s)
end program aiff2riff
