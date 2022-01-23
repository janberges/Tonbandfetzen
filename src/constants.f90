module constants
   use, intrinsic :: iso_fortran_env, only: &
      i1 => int8, &
      i2 => int16, &
      i4 => int32, &
      i8 => int64, &
      eof => iostat_end, &
      eol => iostat_eor, &
      stdin => input_unit, &
      stdout => output_unit, &
      stderr => error_unit
   implicit none
   private

   public :: audio, dp, eof, eol, i1, i2, i2max, i4, i8, pi, &
      stderr, stdin, stdout

   integer, parameter :: dp = selected_real_kind(14, 300)
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
