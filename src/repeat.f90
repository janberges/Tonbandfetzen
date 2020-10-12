program repeat
   use constants
   use riff
   use paths
   use io
   implicit none

   integer :: i
   character(:), allocatable :: path, n
   type(audio) :: s1
   type(audio) :: s

   path = command_argument(1)
   n    = command_argument(2)

   call take(path, s1)

   read (n, *) s%points

   s%channels  = s1%channels
   s%points    = s1%points * s%points
   s%rate      = s1%rate
   s%amplitude = s1%amplitude

   allocate(s%sound(s%channels, s%points))

   do i = 1, s%points, s1%points
      s%sound(:, i:i + s1%points) = s1%sound
   end do

   call make(stem(path) // 'x' // n // '.wav', s)
end program repeat
