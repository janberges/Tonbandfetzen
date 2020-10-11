program noise
   use constants
   use riff
   use paths
   use io
   implicit none

   integer :: i, t, n, count
   character(:), allocatable :: arg
   real(dp) :: frequency, width, duration
   real(dp) :: omega, phi
   real(dp), allocatable :: sound(:)
   type(audio) :: s

   n = command_argument_count()

   if (n .gt. 1) then
      arg = command_argument(2)
      read (arg, *) frequency
   else
      frequency = 440.0_dp
   end if

   if (n .gt. 2) then
      arg = command_argument(3)
      read (arg, *) width
   else
      width = 12.0_dp
   end if

   if (n .gt. 3) then
      arg = command_argument(4)
      read (arg, *) duration
   else
      duration = 1.0_dp
   end if

   if (n .gt. 4) then
      arg = command_argument(5)
      read (arg, *) count
   else
      count = 100
   end if

   s%channels = 1
   s%rate     = 44100.0_dp
   s%points   = nint(s%rate * duration)

   allocate(s%sound(s%channels, s%points))

   allocate(sound(s%points))

   sound = 0.0_dp

   do i = 1, count
      call random_number(phi)
      phi = 2.0_dp * pi * phi

      call random_number(omega)
      omega = 2.0_dp * pi * frequency / s%rate &
         * 2 ** (width * (0.5_dp - omega) / 12)

      do t = 1, s%points
         sound(t) = sound(t) &
            + sin(phi + omega * t) * (t - 1) &
            + sin(phi + omega * (t + s%points)) * (s%points - t)
      end do
   end do

   sound = sound * i2max / maxval(abs(sound))

   s%sound(1, :) = nint(sound, i2)

   arg = command_argument(1)

   call make(arg, s)
end program noise
