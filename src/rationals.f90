module rationals
   use constants, only: dp
   implicit none
   private

   public :: rational

contains

   function rational(ratio)
      real(dp) :: rational

      character(*), intent(in) :: ratio

      integer :: i, error
      real(dp) :: numerator, denominator

      i = scan(ratio, ':/')

      if (i .eq. 0) then
         read (ratio, *, iostat=error) rational
         if (error .ne. 0) rational = 0.0_dp
      else
         read (ratio(:i - 1), *, iostat=error) numerator
         if (error .ne. 0) numerator = 0.0_dp

         read (ratio(i + 1:), *, iostat=error) denominator
         if (error .ne. 0) denominator = 1.0_dp

         rational = numerator / denominator
      end if
   end function rational
end module rationals
