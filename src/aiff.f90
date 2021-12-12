module aiff
   use constants
   use extended
   implicit none
   private

   public :: read_aiff, write_aiff

   interface re
      module procedure reverse_bytes_i2, reverse_bytes_i4
   end interface re

contains

   subroutine read_aiff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s

      character(1) :: byte
      character(4) :: ckID, formType
      character(10) :: extended
      integer, parameter :: unit = 14
      integer :: i, error
      integer(i4) :: ckSize, offset, blockSize
      integer(i2) :: sampleSize

      open(unit, file=file, action='read', status='old', &
         form='unformatted', access='stream')

      do
         read (unit, iostat=error) ckID, ckSize
         if (error .eq. eof) exit

         ckSize = re(ckSize)

         select case (ckID)
            case ('FORM')
               read (unit) formType

            case ('COMM')
               read (unit) s%channels, s%points, sampleSize, extended

               s%channels = re(s%channels)
               s%points   = re(s%points)
               sampleSize = re(sampleSize)
               s%rate     = decode(extended)

               if (sampleSize .ne. 16_i2) then
                  write (stderr, "('Error: only 16 bits supported')")
                  stop
               end if

            case ('SSND')
               allocate(s%sound(s%channels, s%points))
               read (unit) offset, blockSize, s%sound

               offset    = re(offset)
               blockSize = re(blockSize)
               s%sound   = re(s%sound)

            case ('APPL')
               read (unit) extended
               s%amplitude = decode(extended)

            case default
               read (unit) (byte, i = 1, ckSize)
         end select
      end do

      close(unit)
   end subroutine read_aiff

   subroutine write_aiff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(in) :: s

      integer, parameter :: unit = 15
      integer(i4), parameter :: commSize = 18_i4, applSize = 10_i4
      integer(i4), parameter :: offset = 0_i4, blockSize = 0_i4
      integer(i4) :: formSize, ssndSize
      integer(i2), parameter :: sampleSize = 16_i2
      integer(i2) :: blockAlign

      blockAlign = 2_i2 * s%channels

      ssndSize = 8_i4 + blockAlign * s%points
      formSize = 4_i4 + 8_i4 + commSize + 8_i4 + ssndSize

      if (s%amplitude .ne. 1.0_dp) then
         formSize = formSize + 8_i4 + applSize
      end if

      open(unit, file=file, action='write', status='replace', &
         form='unformatted', access='stream')

      write (unit) 'FORM', re(formSize)
      write (unit) 'AIFF'
      write (unit) 'COMM', re(commSize)
      write (unit) re(s%channels), re(s%points), re(sampleSize), encode(s%rate)
      write (unit) 'SSND', re(ssndSize)
      write (unit) re(offset), re(blockSize), re(s%sound)

      if (s%amplitude .ne. 1.0_dp) then
         write (unit) 'APPL', re(applSize)
         write (unit) encode(s%amplitude)
      end if

      close(unit)
   end subroutine write_aiff

   elemental function reverse_bytes_i2(original) result(reversed)
      integer(i2), intent(in) :: original
      integer(i2) :: reversed
      integer(i1) :: bytes(2)

      bytes = transfer(original, bytes)
      bytes = bytes(2:1:-1)
      reversed = transfer(bytes, reversed)
   end function reverse_bytes_i2

   elemental function reverse_bytes_i4(original) result(reversed)
      integer(i4), intent(in) :: original
      integer(i4) :: reversed
      integer(i1) :: bytes(4)

      bytes = transfer(original, bytes)
      bytes = bytes(4:1:-1)
      reversed = transfer(bytes, reversed)
   end function reverse_bytes_i4
end module aiff
