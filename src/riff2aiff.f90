program riff2aiff
   use aiff
   use constants
   use io
   use riff
   implicit none

   type(audio) :: s

   call read_riff(command_argument(-2, '/dev/stdin'), s)
   call write_aiff(command_argument(-1, '/dev/stdout'), s)
end program riff2aiff
