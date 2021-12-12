module riff
   use bytes
   use constants
   use extended
   implicit none
   private

   public :: read_riff, write_riff

contains

   subroutine read_riff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s

      character(1) :: byte
      character(4) :: ckID, formType
      character(10) :: extended
      integer, parameter :: unit = 14
      integer :: i, error
      integer(i4) :: ckSize, sampleRate, byteRate
      integer(i2) :: sampleSize, formatTag, blockAlign

      open(unit, file=file, action='read', status='old', &
         form='unformatted', access='stream')

      do
         read (unit, iostat=error) ckID, ckSize
         if (error .eq. eof) exit

         select case (ckID)
            case ('RIFF')
               read (unit) formType

            case ('fmt ')
               read (unit) formatTag, s%channels, sampleRate, byteRate
               read (unit) blockAlign, sampleSize

               s%rate = real(sampleRate, dp)

               if (sampleSize .ne. 16_i2) then
                  write (stderr, "('Error: only 16 bits supported')")
                  stop
               end if

            case ('data')
               s%points = ckSize / (2 * s%channels)
               allocate(s%sound(s%channels, s%points))
               read (unit) s%sound

            case ('APPL')
               read (unit) extended
               s%amplitude = decode(extended)

            case default
               read (unit) (byte, i = 1, ckSize)
         end select
      end do

      close(unit)
   end subroutine read_riff

   subroutine write_riff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(in) :: s

      integer, parameter :: unit = 15
      integer(i4), parameter :: fmtSize = 16_i4, applSize = 10_i4
      integer(i4) :: riffSize, dataSize, sampleRate, byteRate
      integer(i2), parameter :: sampleSize = 16_i2, formatTag = 1_i2
      integer(i2) :: blockAlign

      blockAlign = 2_i2 * s%channels
      sampleRate = nint(s%rate, i4)
      byteRate = blockAlign * sampleRate

      dataSize = blockAlign * s%points
      riffSize = 4_i4 + 8_i4 + fmtSize + 8_i4 + dataSize

      if (s%amplitude .ne. 1.0_dp) then
         riffSize = riffSize + 8_i4 + applSize
      end if

      if (file .eq. 'stdout') then
         write (stdout, '(10000000000A)', advance='no') &
            'RIFF', c(riffSize), 'WAVE', &
            'fmt ', c(fmtSize), c(formatTag), c(s%channels), &
            c(sampleRate), c(byteRate), c(blockAlign), c(sampleSize), &
            'data', c(dataSize), c(s%sound)

         if (s%amplitude .ne. 1.0_dp) then
            write (stdout, '(3A)', advance='no') &
               'APPL', c(applSize), encode(s%amplitude)
         end if
      else
         open(unit, file=file, action='write', status='replace', &
            form='unformatted', access='stream')

         write (unit) 'RIFF', riffSize, 'WAVE', &
            'fmt ', fmtSize, formatTag, s%channels, &
            sampleRate, byteRate, blockAlign, sampleSize, &
            'data', dataSize, s%sound

         if (s%amplitude .ne. 1.0_dp) then
            write (unit) 'APPL', applSize, encode(s%amplitude)
         end if

         close(unit)
      end if
   end subroutine write_riff
end module riff
