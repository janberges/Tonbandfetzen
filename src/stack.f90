program stack
   use constants
   use riff
   use io
   implicit none

   integer :: i, n, t
   integer(i2) :: c
   real(dp), allocatable :: sound(:, :)
   type(audio), allocatable :: p(:)
   type(audio) :: s

   s%channels = 1
   s%points   = 0
   s%rate     = 0.0_dp

   n = command_argument_count()

   allocate(p(n - 1))

   do i = 1, n - 1
      call take(command_argument(i), p(i))

      s%channels = max(s%channels, p(i)%channels)
      s%points   = max(s%points,   p(i)%points)
      s%rate     = max(s%rate,     p(i)%rate)
   end do

   allocate(s%sound(0:s%channels - 1, 0:s%points - 1))
   allocate(  sound(0:s%channels - 1, 0:s%points - 1))

   sound(:, :) = 0.0_dp

   do i = 1, n - 1
      do c = 1, s%channels
         do t = 1, s%points
            sound(c, t) = sound(c, t) + p(i)%amplitude &
               * p(i)%sound(modulo(c, p(i)%channels), modulo(t, p(i)%points))
         end do
      end do
   end do

   sound(:, :) = sound / i2max

   s%amplitude = maxval(abs(sound))
   s%sound = nint(i2max / s%amplitude * sound, i2)

   call make(command_argument(n), s)
end program stack
