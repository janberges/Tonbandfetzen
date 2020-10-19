module io
   use constants
   implicit none
   private

   public :: slurp, command_argument

contains

   function slurp(file) result(content)
      character(:), allocatable :: content

      character(*), intent(in) :: file

      integer, parameter :: unit = 16
      integer :: size, stat

      open(unit, file=file, action='read', status='old', &
         form='unformatted', access='stream', iostat=stat)

      if (stat .ne. 0) then
         write (stderr, "('Error reading file ''', A, '''.')") file
         stop
      end if

      inquire(unit, size=size)

      allocate(character(size) :: content)

      read (unit) content

      close(unit)
   end function slurp

   function command_argument(i) result(argument)
      character(:), allocatable :: argument

      integer, intent(in) :: i

      integer :: size, stat

      call get_command_argument(i, length=size, status=stat)

      if (stat .ne. 0) then
         write (stderr, "('Error: argument ', I0, ' missing.')") i
         stop
      end if

      allocate(character(size) :: argument)

      call get_command_argument(i, value=argument)
   end function command_argument
end module io
