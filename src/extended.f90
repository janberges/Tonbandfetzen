module extended
   use constants, only: dp
   implicit none
   private

   public :: decode, encode

contains

   function decode(code) result(x)
      real(dp) :: x

      character(*), intent(in) :: code

      integer :: byte, bytes(10), bit, digit, sgn, power
      real(dp) :: mantissa

      do byte = 1, 10
         bytes(byte) = ichar(code(byte:byte))
      end do

      sgn = merge(-1, 1, btest(bytes(1), 7))
      mantissa = 0.0_dp
      power = -16383

      digit = 0
      do byte = 2, 1, -1
         do bit = 0, 5 + byte
            if (btest(bytes(byte), bit)) then
               power = power + 2 ** digit
            end if
            digit = digit + 1
         end do
      end do

      digit = 0
      do byte = 3, 10
         do bit = 7, 0, -1
            if (btest(bytes(byte), bit)) then
               mantissa = mantissa + 2.0_dp ** digit
            end if
            digit = digit - 1
         end do
      end do

      x = sgn * mantissa * 2.0_dp ** power
   end function decode

   function encode(x) result(code)
      character(10) :: code

      real(dp), intent(in) :: x

      integer :: byte, bytes(10), bit, power
      real(dp) :: mantissa

      power = int(log(abs(x)) / log(2.0_dp))
      mantissa = abs(x) / 2.0_dp ** power
      power = power + 16383

      bytes = 0

      if (x .lt. 0) bytes(1) = ibset(bytes(1), 7)

      do byte = 2, 1, -1
         do bit = 0, 5 + byte
            if (modulo(power, 2) .eq. 1) then
               bytes(byte) = ibset(bytes(byte), bit)
            end if
            power = power / 2
         end do
      end do

      do byte = 3, 10
         do bit = 7, 0, -1
            if (mantissa .ge. 1.0_dp) then
               mantissa = mantissa - 1.0_dp
               bytes(byte) = ibset(bytes(byte), bit)
            end if
            mantissa = 2.0_dp * mantissa
         end do
      end do

      do byte = 1, 10
         code(byte:byte) = char(bytes(byte))
      end do
   end function encode
end module extended
