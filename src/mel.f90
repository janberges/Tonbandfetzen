program mel
   use constants
   use interpreter
   use io
   use riff
   implicit none

   type(audio) :: music

   call play(slurp(command_argument(1)), music)
   call write_riff(command_argument(2), music)
end program mel
