module io
   use constants
   implicit none
   private

   public :: slurp, command_argument, environment_variable

contains

   function slurp(file) result(content)
      character(:), allocatable :: content

      character(*), intent(in) :: file

      integer, parameter :: unit = 16
      integer :: i, stat

      open(unit, file=file, action='read', status='old', &
         form='unformatted', access='stream', iostat=stat)

      if (stat .ne. 0) then
         write (stderr, "('Error reading file ''', A, '''.')") file
         stop
      end if

      allocate(character(1048576) :: content)

      do i = 1, len(content)
         read (unit, iostat=stat) content(i:i)
         if (stat .eq. eof) then
            content = content(1:i - 1)
            exit
         end if
      end do

      close(unit)
   end function slurp

   function command_argument(num, def) result(arg)
      character(:), allocatable :: arg

      integer, intent(in) :: num
      character(*), intent(in) :: def

      integer :: i, n, size

      i = num
      n = command_argument_count()

      if (i .lt. 0) i = n + i + 1

      if (i .lt. 1 .or. i .gt. n) then
         arg = def
      else
         call get_command_argument(i, length=size)

         allocate(character(size) :: arg)

         call get_command_argument(i, value=arg)

         if (arg .eq. '-') arg = def
      end if
   end function command_argument

   function environment_variable(name) result(value)
      character(:), allocatable :: value

      character(*), intent(in) :: name

      integer :: size

      call get_environment_variable(name, length=size)

      allocate(character(size) :: value)

      call get_environment_variable(name, value=value)
   end function environment_variable
end module io
