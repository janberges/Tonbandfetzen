program riff2aiff
   use aiff, only: write_aiff
   use constants, only: audio
   use io, only: command_argument
   use riff, only: read_riff
   implicit none

   type(audio) :: s

   call read_riff(command_argument(-2, '/dev/stdin'), s)
   call write_aiff(command_argument(-1, '/dev/stdout'), s)
end program riff2aiff
