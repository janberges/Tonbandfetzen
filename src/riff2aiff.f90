program riff2aiff
   use aiff
   use constants
   use io
   use riff
   implicit none

   type(audio) :: s

   if (command_argument_count() .ne. 2) then
      write (*, "('Usage: riff2aiff <infile> <outfile>')")
      write (*, "('See ''man riff2aiff'' for more information.')")
      stop
   end if

   call read_riff(command_argument(1), s)
   call write_aiff(command_argument(2), s)
end program riff2aiff
