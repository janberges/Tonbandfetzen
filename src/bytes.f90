module bytes
   use constants, only: i1, i2, i4
   implicit none
   private

   public :: c, r

   interface c
      module procedure chars_i2, chars_i4
   end interface c

   interface r
      module procedure reverse_i2, reverse_i4
   end interface r

contains

   elemental function chars_i2(i) result(o)
      integer(i2), intent(in) :: i
      character(2) :: o

      o = transfer(i, o)
   end function chars_i2

   elemental function chars_i4(i) result(o)
      integer(i4), intent(in) :: i
      character(4) :: o

      o = transfer(i, o)
   end function chars_i4

   elemental function reverse_i2(i) result(o)
      integer(i2), intent(in) :: i
      integer(i2) :: o
      integer(i1) :: j(2)

      j = transfer(i, j)
      j = j(2:1:-1)
      o = transfer(j, o)
   end function reverse_i2

   elemental function reverse_i4(i) result(o)
      integer(i4), intent(in) :: i
      integer(i4) :: o
      integer(i1) :: j(4)

      j = transfer(i, j)
      j = j(4:1:-1)
      o = transfer(j, o)
   end function reverse_i4
end module bytes
