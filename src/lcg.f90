! https://en.wikipedia.org/wiki/MINSTD

module lcg
   use constants
   implicit none
   private

   public :: minstd

   integer(i8), parameter :: a = 48271_i8, m = 2147483647_i8
   integer(i8), save :: i = 1_i8
   real(dp), parameter :: s = 1.0_dp / m

contains

   subroutine minstd(r)
      real(dp), intent(out) :: r

      i = modulo(i * a, m)
      r = s * i
   end subroutine minstd
end module lcg
