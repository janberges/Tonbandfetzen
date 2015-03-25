program ratios
   use io
   use rationals
   use search
   implicit none

   integer :: i
   character(:), allocatable :: file, numeral

   do i = 1, command_argument_count()
      file = command_argument(i)

      write (*, "('looking for rationals: ', A)") file

      call focus(file)

      do
         numeral = next('0123456789.:')
         if (numeral .eq. 'none') exit

         write (*, "(ES8.3E1, ' <- ', A)") rational(numeral), numeral
      end do
   end do
end program ratios
