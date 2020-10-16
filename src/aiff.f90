module aiff
   use constants
   use extended
   implicit none
   private

   public :: read_aiff, write_aiff

contains

   subroutine read_aiff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s

      character(1) :: byte
      character(4) :: ckID, formType
      character(10) :: extended
      integer, parameter :: unit = 14
      integer :: i, stat
      integer(i4) :: ckSize, offset, blockSize
      integer(i2) :: sampleSize

      open(unit, file=file, action='read', status='old', &
         form='unformatted', access='stream', iostat=stat, &
         convert='big_endian')

      if (stat .ne. 0) then
         write (*, "('Error reading file ''', A, '''.')") file
         stop
      end if

      do
         read (unit, iostat=stat) ckID, ckSize
         if (stat .eq. eof) exit

         select case (ckID)
            case ('FORM')
               read (unit) formType

            case ('COMM')
               read (unit) s%channels, s%points, sampleSize, extended
               s%rate = decode(extended)

               if (sampleSize .ne. 16_i2) then
                  write (*, "('Error: only 16 bits supported')")
                  stop
               end if

            case ('SSND')
               allocate(s%sound(s%channels, s%points))
               read (unit) offset, blockSize, s%sound

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

      integer :: stat
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
         form='unformatted', access='stream', iostat=stat, &
         convert='big_endian')

      if (stat .ne. 0) then
         write (*, "('Error writing file ''', A, '''.')") file
         stop
      end if

      write (unit) 'FORM', formSize
      write (unit) 'AIFF'
      write (unit) 'COMM', commSize
      write (unit) s%channels, s%points, sampleSize, encode(s%rate)
      write (unit) 'SSND', ssndSize
      write (unit) offset, blockSize, s%sound

      if (s%amplitude .ne. 1.0_dp) then
         write (unit) 'APPL', applSize
         write (unit) encode(s%amplitude)
      end if

      close(unit)
   end subroutine write_aiff
end module aiff
