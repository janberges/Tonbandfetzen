module search
   use constants
   implicit none
   private

   public :: focus, reset, next, remember, revert, set, get

   character(:), allocatable :: sequence
   integer(i2), allocatable :: info(:)

   integer, save :: last, marks(0:99)

contains

      subroutine focus(it)
         character(*), intent(in) :: it

         sequence = it

         allocate(info(len(sequence)))

         call reset
      end subroutine focus

      subroutine reset
         last = 0
         marks = 0
         info = 0
      end subroutine reset

      function next(set)
         character(:), allocatable :: next

         character(*), intent(in) :: set

         integer :: first

         first = scan(sequence(last + 1:), set)

         if (first .eq. 0) then
            next = 'none'
            return
         end if

         first = first + last

         last = verify(sequence(first + 1:), set)

         if (last .eq. 0) then
            last = len(sequence)
         else
            last = last + first - 1
         end if

         next = sequence(first:last)
      end function next

      subroutine remember(mark)
         integer, intent(in) :: mark

         marks(mark) = last
      end subroutine remember

      subroutine revert(mark)
         integer, intent(in) :: mark

         last = marks(mark)
      end subroutine revert

      subroutine set(i)
         integer, intent(in) :: i

         info(last) = int(i, i2)
      end subroutine set

      subroutine get(i)
         integer, intent(out) :: i

         i = info(last)
      end subroutine get
end module search
