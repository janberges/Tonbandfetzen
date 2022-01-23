program aiff2riff
   use aiff, only: read_aiff
   use constants, only: audio
   use io, only: command_argument
   use riff, only: write_riff
   implicit none

   type(audio) :: s

   call read_aiff(command_argument(-2, '/dev/stdin'), s)
   call write_riff(command_argument(-1, '/dev/stdout'), s)
end program aiff2riff
