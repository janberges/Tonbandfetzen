program mel
   use constants
   use interpreter
   use io
   use riff
   implicit none

   integer :: n
   type(audio) :: music

   n = command_argument_count()

   call play(slurp(command_argument(-2, '/dev/stdin')), music)
   call write_riff(command_argument(-1, '/dev/stdout'), music)
end program mel
