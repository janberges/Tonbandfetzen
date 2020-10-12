program reverse
   use constants
   use io
   use paths
   use riff
   implicit none

   integer :: i
   character(:), allocatable :: path
   type(audio) :: s

   do i = 1, command_argument_count()
      path = command_argument(i)

      call read_riff(path, s)

      s%sound(:, :) = s%sound(:, s%points:1:-1)

      call write_riff(stem(path) // '_reversed.wav', s)
   end do
end program reverse
