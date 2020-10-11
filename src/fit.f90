program fit
   use constants
   use riff
   use paths
   use io
   implicit none

   integer :: t, t0, n
   character(:), allocatable :: path, arg
   real(dp) :: t1, dt, duration, scale
   type(audio) :: s0, s

   path = command_argument(1)

   call take(path, s0)

   n = command_argument_count()

   if (n .gt. 1) then
      arg = command_argument(2)
      read (arg, *) duration
   else
      duration = real(s0%points, dp) / real(s0%rate, dp)
   end if

   if (n .gt. 2) then
      arg = command_argument(3)
      read (arg, *) s%amplitude
   else
      s%amplitude = s0%amplitude
   end if

   if (n .gt. 3) then
      arg = command_argument(4)
      read (arg, *) s%rate
   else
      s%rate = s0%rate
   end if

   s%channels = s0%channels
   s%points = nint(abs(duration) * s%rate)

   allocate(s%sound(s0%channels, s%points))

   scale = real(s0%points - 1, dp) / real(s%points - 1, dp)

   do t = 0, s%points - 1
      t1 = 1 + scale * t
      t0 = floor(t1)
      dt = t1 - t0
      s%sound(:, 1 + t) = nint(s0%sound(:, t0) * (1 - dt) &
         + s0%sound(:, t0 + 1) * dt, i2)
   end do

   if (duration .lt. 0.0_dp) s%sound(:, :) = s%sound(:, s%points:1:-1)

   call make(stem(path) // '_fitted.wav', s)
end program fit
