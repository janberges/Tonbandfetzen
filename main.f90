program tonbandfetzen
   use aiff
   use rationals
   use search
   implicit none

   type(audio) :: s

   integer :: i, l
   character(20) :: x
   character(:), allocatable :: n

   do i = 1, command_argument_count()
      call get_command_argument(i, x, l)

      select case(x(l - 3:l))
         case ('.aif')
            call take(x(:l), s)

            s%sound(:, :) = s%sound(:, s%points:1:-1)

            call make(x(:l - 4) // '_reversed' // x(l - 3:l), s)

         case ('.txt')
            call focus(x(:l))

            do
               n = next('0123456789.:')
               if (n .eq. 'none') exit

               write (*, "(ES8.3E1, ' <- ', A)") rational(n), n
            end do
      end select
   end do
end program tonbandfetzen
