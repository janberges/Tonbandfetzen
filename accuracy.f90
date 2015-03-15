module accuracy
   use, intrinsic :: iso_fortran_env, only: int16, int32
   implicit none
   private

   public :: dp, i2, i4

   integer, parameter :: dp = selected_real_kind(14, 300)
   integer, parameter :: i2 = int16, i4 = int32
end module accuracy
