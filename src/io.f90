module io
   use constants, only: eof, eol, stderr
   implicit none
   private

   public :: slurp, command_argument, environment_variable

contains

   function slurp(file) result(content)
      character(:), allocatable :: content

      character(*), intent(in) :: file

      integer :: unit, i, error
      logical :: f

      character, parameter :: lf = new_line('A')

      f = file .ne. 'stdin'

      if (f) then
         open (newunit=unit, file=file, iostat=error, &
            action='read', status='old', access='stream')

         if (error .ne. 0) then
            write (stderr, "('Error: Cannot read file ''', A, '''.')") file
            stop
         end if
      end if

      allocate(character(1048576) :: content)

      do i = 1, len(content)
         if (f) then
            read (unit, iostat=error) content(i:i)
         else
            read (*, '(A1)', iostat=error, advance='no') content(i:i)
            if (error .eq. eol) content(i:i) = lf
         end if
         if (error .eq. eof) then
            content = content(1:i - 1)
            exit
         end if
      end do

      if (f) close (unit)
   end function slurp

   function command_argument(num, def) result(arg)
      character(:), allocatable :: arg

      integer, intent(in) :: num
      character(*), intent(in) :: def

      integer :: i, n, size

      i = num
      n = command_argument_count()

      if (-n .lt. i .and. i .lt. 0) i = i + n

      i = i + 1

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

      if (len(value) .gt. 0) then
         call get_environment_variable(name, value=value)
      end if
   end function environment_variable
end module io
