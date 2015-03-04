module extended
   use accuracy
   implicit none
   private

   public :: decode, encode

contains

   function decode(code) result(value)
      real(dp) :: value

      character(*), intent(in) :: code

      integer :: byte, bytes(10), bit, digit, sign, exponent
      real(dp) :: mantissa

      do byte = 1, 10
         bytes(byte) = ichar(code(byte:byte))
      end do

      sign = merge(-1, 1, btest(bytes(1), 7))
      mantissa = 0.0_dp
      exponent = -16383

      digit = 0
      do byte = 2, 1, -1
         do bit = 0, 5 + byte
            if (btest(bytes(byte), bit)) then
               exponent = exponent + 2 ** digit
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

      value = sign * mantissa * 2.0_dp ** exponent
   end function decode

   function encode(value) result(code)
      character(10) :: code

      real(dp), intent(in) :: value

      integer :: byte, bytes(10), bit, exponent
      real(dp) :: mantissa

      exponent = int(log(max(abs(value), 1.0_dp)) / log(2.0_dp))
      mantissa = abs(value) / 2.0_dp ** exponent
      exponent = exponent + 16383

      bytes(:) = 0

      if (value .lt. 0) bytes(1) = ibset(bytes(1), 7)

      do byte = 2, 1, -1
         do bit = 0, 5 + byte
            if (modulo(exponent, 2) .eq. 1) then
               bytes(byte) = ibset(bytes(byte), bit)
            end if
            exponent = exponent / 2
         end do
      end do

      do byte = 3, 10
         do bit = 7, 0, -1
            if (mantissa .ge. 1) then
               mantissa = mantissa - 1
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
