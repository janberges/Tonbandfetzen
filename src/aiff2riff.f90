program aiff2riff
   use aiff
   use constants
   use io
   use riff
   implicit none

   type(audio) :: s

   call read_aiff(command_argument(-2, '/dev/stdin'), s)
   call write_riff(command_argument(-1, '/dev/stdout'), s)
end program aiff2riff
