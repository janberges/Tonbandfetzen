module io
   implicit none
   private

   public :: slurp

contains

   function slurp(file) result(content)
      character(*), intent(in) :: file

      character(:), allocatable :: content

      integer, parameter :: unit = 16
      integer :: size

      open(unit,                  &
         &    file=file,          &
         &  action='read',        &
         &  status='old',         &
         &    form='unformatted', &
         &  access='stream')

      inquire(unit, size=size)

      allocate(character(size) :: content)

      read (unit) content

      close(unit)
   end function slurp
end module io
