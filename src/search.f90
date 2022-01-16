module search
   use constants
   implicit none
   private

   public :: focus, reset, next, remember, revert, known, set, get, &
      numeral, lexical, special

   character(:), allocatable :: sequence
   integer(i2), allocatable :: info(:)

   integer, save :: last, marks(0:99)

   character(*), parameter :: &
      numeral = '.0123456789:', &
      lexical = 'abcdefghijklmnopqrstuvwxyz#', &
      special = '!"$%&''()*+,-/;<=>?@ABCDEFGHIJMNOPQRSTUVWXYZ[\]^_`{|}~'

   character(*), parameter :: canonical = numeral // lexical // special

contains

   subroutine focus(it)
      character(*), intent(in) :: it

      sequence = it

      if (allocated(info)) deallocate(info)
      allocate(info(len(sequence)))

      call reset
   end subroutine focus

   subroutine reset
      last = 0
      marks = -1
      info = 0
   end subroutine reset

   function next(set, def, length, barrier)
      character(:), allocatable :: next

      character(*), intent(in) :: set
      character(*), intent(in), optional :: def
      integer, intent(in), optional :: length
      character(*), intent(in), optional :: barrier

      integer :: first, break

      character(:), allocatable :: blocking

      if (present(barrier)) then
         blocking = barrier
      else if (present(def)) then
         blocking = canonical
      else
         blocking = special
      end if

      first = scan(sequence(last + 1:), set)
      break = scan(sequence(last + 1:), blocking)

      if (first .eq. 0 .or. break .ne. 0 .and. break .lt. first) then
         if (present(def)) then
            next = def
         else
            next = 'none'
         end if
         return
      end if

      first = first + last

      if (present(length)) then
         last = first + length - 1
      else
         last = verify(sequence(first + 1:), set)

         if (last .eq. 0) then
            last = len(sequence)
         else
            last = last + first - 1
         end if
      end if

      next = sequence(first:last)
   end function next

   subroutine remember(mark)
      integer, intent(in) :: mark

      marks(mark) = last
   end subroutine remember

   subroutine revert(mark)
      integer, intent(in) :: mark

      if (known(mark)) last = marks(mark)
   end subroutine revert

   function known(mark)
      logical :: known
      integer, intent(in) :: mark

      known = marks(mark) .ne. -1
   end function known

   subroutine set(i)
      integer, intent(in) :: i

      info(last) = int(i, i2)
   end subroutine set

   subroutine get(i)
      integer, intent(out) :: i

      i = info(last)
   end subroutine get
end module search
