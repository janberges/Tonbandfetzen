program flange
   use constants
   use riff
   use paths
   use io
   implicit none

   integer :: i
   character(:), allocatable :: path, ampl
   real(dp) :: amplitude
   type(audio) :: s1
   type(audio) :: s

   path = command_argument(1)
   ampl = command_argument(2)

   call take(path, s1)

   read (ampl, *) amplitude

   s%channels  = s1%channels
   s%points    = s1%points
   s%rate      = s1%rate
   s%amplitude = s1%amplitude

   allocate(s%sound(s%channels, s%points))

   do i = 0, s%points - 1
      s%sound(:, i) = s1%sound(:, i &
         + nint(amplitude * s%rate * sin(2 * pi * i / (s%points - 1)) ** 2))
   end do

   call make(stem(path) // '_flanged.wav', s)
end program flange
