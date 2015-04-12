module constants
   use, intrinsic :: iso_fortran_env, only: int16, int32
   implicit none
   private

   public :: dp, i2, i4, pi

   integer, parameter :: dp = selected_real_kind(14, 300)
   integer, parameter :: i2 = int16, i4 = int32

   real(dp), parameter :: pi = 4.0_dp * atan(1.0_dp)
end module constants
