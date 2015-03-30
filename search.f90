module search
   implicit none
   private

   public :: focus, reset, next

   character(:), allocatable :: sequence

   integer, save :: first = 0, last = 0

contains

      subroutine focus(it)
         character(*), intent(in) :: it

         sequence = it

         call reset
      end subroutine focus

      subroutine reset
         first = 0
         last = 0
      end subroutine reset

      function next(set)
         character(:), allocatable :: next

         character(*), intent(in) :: set

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
end module search
