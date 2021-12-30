program stack
   use constants
   use io
   use riff
   implicit none

   integer :: i, n, t
   integer(i2) :: c
   real(dp), allocatable :: sound(:, :)
   type(audio), allocatable :: p(:)
   type(audio) :: s

   s%channels = 1
   s%points = 0
   s%rate = 1.0_dp
   s%amplitude = 0.0_dp

   n = command_argument_count()

   allocate(p(n - 1))

   do i = 1, n - 1
      call read_riff(command_argument(i, '/dev/stdin'), p(i))

      s%channels = max(s%channels, p(i)%channels)
      s%points = max(s%points, p(i)%points)
      s%rate = max(s%rate, p(i)%rate)
   end do

   allocate(s%sound(s%channels, s%points))
   allocate(sound(s%channels, s%points))

   sound(:, :) = 0.0_dp

   do i = 1, n - 1
      do c = 1, s%channels
         do t = 1, s%points
            sound(c, t) = sound(c, t) + p(i)%amplitude * p(i)%sound( &
               1_i2 + modulo(c - 1_i2, p(i)%channels), &
               1_i2 + modulo(t - 1_i2, p(i)%points))
         end do
      end do
   end do

   sound = sound / i2max

   if (s%points .gt. 0) then
      s%amplitude = maxval(abs(sound))
      s%sound = nint(i2max / s%amplitude * sound, i2)
   end if

   call write_riff(command_argument(-1, '/dev/stdout'), s)
end program stack
