program stretch
   use constants
   use io
   use rationals
   use riff
   implicit none

   integer :: t, t0
   real(dp) :: t1, dt, scaling, factor
   type(audio) :: s0, s

   factor = rational(command_argument(1, '-1'))

   call read_riff(command_argument(2, '/dev/stdin'), s0)

   s%channels  = s0%channels
   s%points    = nint(abs(factor) * s0%points)
   s%rate      = s0%rate
   s%amplitude = s0%amplitude

   if (s%points .eq. s0%points) then
      s%sound = s0%sound
   else
      allocate(s%sound(s0%channels, s%points))

      scaling = real(s0%points - 1, dp) / real(max(s%points, 2) - 1, dp)

      do t = 0, s%points - 1
         t1 = 1 + scaling * t
         t0 = floor(t1)
         dt = t1 - t0
         s%sound(:, 1 + t) = nint(s0%sound(:, t0) * (1 - dt) &
            + s0%sound(:, t0 + 1) * dt, i2)
      end do
   end if

   if (factor .lt. 0.0_dp) s%sound(:, :) = s%sound(:, s%points:1:-1)

   call write_riff(command_argument(3, '/dev/stdout'), s)
end program stretch
