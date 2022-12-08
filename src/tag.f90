program tag
   use constants, only: audio
   use id3, only: write_id3
   use io, only: command_argument
   use riff, only: read_riff, write_riff
   implicit none

   type(audio) :: s

   call read_riff(command_argument(2, '/dev/stdin'), s)

   s%meta = write_id3(command_argument(1, 'meta.dat'))

   if (len(s%meta) .eq. 0) deallocate(s%meta)

   call write_riff(command_argument(3, '/dev/stdout'), s)
end program tag
