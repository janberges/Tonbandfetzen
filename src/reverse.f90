program reverse
   use constants
   use riff
   use paths
   use io
   implicit none

   integer :: i
   character(:), allocatable :: path
   type(audio) :: s

   do i = 1, command_argument_count()
      path = command_argument(i)

      call take(path, s)

      s%sound(:, :) = s%sound(:, s%points:1:-1)

      call make(stem(path) // '_reversed.wav', s)
   end do
end program reverse
