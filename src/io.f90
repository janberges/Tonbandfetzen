module io
   implicit none
   private

   public :: slurp, command_argument

contains

   function slurp(file) result(content)
      character(:), allocatable :: content

      character(*), intent(in) :: file

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

   function command_argument(i) result(argument)
      character(:), allocatable :: argument

      integer, intent(in) :: i

      integer :: size

      call get_command_argument(i, length=size)

      allocate(character(size) :: argument)

      call get_command_argument(i, value=argument)
   end function command_argument
end module io
