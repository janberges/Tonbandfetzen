program reverse
   use aiff
   use io
   implicit none

   integer :: i, dot
   character(:), allocatable :: file
   type(audio) :: s

   do i = 1, command_argument_count()
      file = command_argument(i)

      call take(file, s)

      s%sound(:, :) = s%sound(:, s%points:1:-1)

      dot = index(file, '.', back=.true.)

      if (dot .eq. 0) dot = len(file) + 1

      call make(file(:dot - 1) // '_reversed' // file(dot:), s)
   end do
end program reverse
