program mono
   use constants, only: audio, i2
   use io, only: command_argument
   use riff, only: read_riff, write_riff
   implicit none

   integer(i2) :: c
   type(audio) :: m, s

   call read_riff(command_argument(1, '/dev/stdin'), s)

   m%channels = 1_i2
   m%points = s%points
   m%rate = s%rate
   m%amplitude = s%amplitude

   do c = 1, s%channels
      m%sound = s%sound(c:c, :)

      call write_riff(command_argument(c + 1, '/dev/null'), m)
   end do
end program mono
