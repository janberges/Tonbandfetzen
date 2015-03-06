program tonbandfetzen
   use aiff
   use io
   implicit none

   type(audio) :: s

   integer :: i, l
   character(20) :: x

   do i = 1, command_argument_count()
      call get_command_argument(i, x, l)

      select case(x(l - 3:l))
         case ('.aif')
            call take(x(:l), s)

            s%sound(:, :) = s%sound(:, s%points:1:-1)

            call make(x(:l - 4) // '_reversed' // x(l - 3:l), s)

         case ('.txt')
            write (*, '(A)') slurp(x(:l))
      end select
   end do
end program tonbandfetzen
