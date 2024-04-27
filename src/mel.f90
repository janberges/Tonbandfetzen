program mel
   use constants, only: audio
   use interpreter, only: play
   use io, only: command_argument, slurp
   use riff, only: write_riff
   implicit none

   type(audio) :: music

   call play(slurp(command_argument(-2, '/dev/stdin')), music)
   call write_riff(command_argument(-1, '/dev/stdout'), music)
end program mel
