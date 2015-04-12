module intervals
   use constants
   implicit none
   private

   public :: interval

contains

   subroutine interval(x, a, b, n)
      real(dp), intent(out) :: x(:)
      real(dp), intent(in) :: a, b
      integer, intent(in), optional :: n
      !      n:      0      1      2      3
      ! binary:     00     01     10     11
      ! yields:  (a,b)  (a,b]  [a,b)  [a,b]

      integer :: i, j, k

      i = size(x)
      j = 1

      if (present(n)) then
         if (btest(n, 1)) j = j - 1
         if (btest(n, 0)) i = i - 1
      end if

      do k = 1, size(x)
         x(k) = i * a + j * b
         i = i - 1
         j = j + 1
      end do

      x = x / (i + j)
   end subroutine interval
end module intervals
