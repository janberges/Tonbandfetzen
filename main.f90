program tonbandfetzen
   use aiff
   use io
   use rationals
   use search
   implicit none

   type(audio) :: s

   integer :: i
   character(:), allocatable :: x, n

   do i = 1, command_argument_count()
      x = command_argument(i)

      select case(x(len(x) - 3:))
         case ('.aif')
            call take(x, s)

            s%sound(:, :) = s%sound(:, s%points:1:-1)

            call make(x(:len(x) - 4) // '_reversed' // x(len(x) - 3:), s)

         case ('.txt')
            call focus(x)

            do
               n = next('0123456789.:')
               if (n .eq. 'none') exit

               write (*, "(ES8.3E1, ' <- ', A)") rational(n), n
            end do
      end select
   end do
end program tonbandfetzen
