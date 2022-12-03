program trim
   use constants, only: audio, dp, i2max
   use io, only: command_argument
   use rationals, only: rational
   use riff, only: read_riff, write_riff
   implicit none

   integer :: a, i, z
   real(dp) :: threshold
   type(audio) :: s0, s

   threshold = rational(command_argument(1, '0')) * i2max

   call read_riff(command_argument(2, '/dev/stdin'), s0)

   do i = 1, s0%points
      a = i
      if (any(s0%sound(:, a) .gt. threshold)) exit
   end do

   do i = s0%points, a - 1, -1
      z = i
      if (any(s0%sound(:, z) .gt. threshold)) exit
   end do

   s%channels = s0%channels
   s%points = z - a + 1
   s%rate = s0%rate
   s%amplitude = s0%amplitude
   s%sound = s0%sound(:, a:z)

   call write_riff(command_argument(3, '/dev/stdout'), s)
end program trim
