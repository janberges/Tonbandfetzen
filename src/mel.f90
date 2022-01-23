program mel
   use constants, only: audio
   use interpreter, only: play
   use io, only: command_argument, slurp
   use riff, only: write_riff
   implicit none

   integer :: n
   type(audio) :: music

   n = command_argument_count()

   call play(slurp(command_argument(-2, '/dev/stdin')), music)
   call write_riff(command_argument(-1, '/dev/stdout'), music)
end program mel
