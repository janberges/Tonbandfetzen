module constants
   use, intrinsic :: iso_fortran_env, only: &
      dp => real64, &
      eof => iostat_end, &
      eol => iostat_eor, &
      i1 => int8, &
      i2 => int16, &
      i4 => int32, &
      i8 => int64, &
      stderr => error_unit
   implicit none
   public

   integer, parameter :: i2max = 2 ** 15 - 1

   real(dp), parameter :: pi = 4.0_dp * atan(1.0_dp)

   type audio
      integer(i2) :: channels
      integer(i4) :: points

      real(dp) :: rate
      real(dp) :: amplitude = 1.0_dp

      integer(i2), allocatable :: sound(:, :)
   end type audio
end module constants
