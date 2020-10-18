module search
   implicit none
   private

   public :: focus, reset, next, remember, revert

   character(:), allocatable :: sequence

   integer, save :: last = 0, marks(0:99) = 0

contains

      subroutine focus(it)
         character(*), intent(in) :: it

         sequence = it

         call reset
      end subroutine focus

      subroutine reset
         last = 0
         marks = 0
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
end module search
