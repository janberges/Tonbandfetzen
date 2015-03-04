program tonbandfetzen
   use aiff
   implicit none

   type(audio) :: s

   integer :: i, l
   character(20) :: x

   do i = 1, command_argument_count()
      call get_command_argument(i, x, l)

      call take(x(:l), s)

      s%sound(:, :) = s%sound(:, s%points:1:-1)

      call make('reversed_' // x(:l), s)
   end do
end program tonbandfetzen
