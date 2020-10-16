program mel
   use constants
   use interpreter
   use io
   use riff
   implicit none

   integer :: n
   type(audio) :: music

   n = command_argument_count()

   if (n .lt. 2) then
      write (*, "('Usage: mel [wavefile ...] <infile> <outfile>')")
      write (*, "('See ''man mel'' for more information.')")
      stop
   end if

   call play(slurp(command_argument(n - 1)), music)
   call write_riff(command_argument(n), music)
end program mel
