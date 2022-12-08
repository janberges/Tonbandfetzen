module riff
   use bytes, only: c
   use constants, only: audio, dp, eof, i2, i4, stderr
   use extended, only: decode, encode
   implicit none
   private

   public :: read_riff, write_riff

contains

   subroutine read_riff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s

      integer :: unit
      integer :: i, error
      character(1) :: byte
      character(4) :: ckID, formType
      character(10) :: extended
      integer(i4) :: ckSize, sampleRate, byteRate
      integer(i2) :: sampleSize, formatTag, blockAlign

      open (newunit=unit, file=file, &
         action='read', status='old', access='stream')

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
               write (stderr, "('Error: Only 16 bits supported.')")
               stop
            end if

         case ('data')
            s%points = ckSize / (2 * s%channels)
            allocate(s%sound(s%channels, s%points))
            read (unit) s%sound

         case ('APPL')
            read (unit) extended
            s%amplitude = decode(extended)

         case ('ID3 ', 'id3 ')
            allocate(character(ckSize) :: s%meta)
            read (unit) s%meta

         case default
            read (unit) (byte, i = 1, ckSize)
         end select
      end do

      close (unit)
   end subroutine read_riff

   subroutine write_riff(filex, s)
      character(*), intent(in) :: filex
      type(audio), intent(in) :: s

      integer :: unit
      logical :: appl
      character(:), allocatable :: file
      integer(i4), parameter :: fmtSize = 16_i4, applSize = 10_i4
      integer(i4) :: riffSize, dataSize, sampleRate, byteRate
      integer(i2), parameter :: sampleSize = 16_i2, formatTag = 1_i2
      integer(i2) :: blockAlign

      if (filex(len(filex):len(filex)) .eq. '/') then
         file = filex(:len(filex) - 1)
         appl = .false.
      else
         file = filex
         appl = s%amplitude .ne. 1.0_dp
      end if

      blockAlign = 2_i2 * s%channels
      sampleRate = nint(s%rate, i4)
      byteRate = blockAlign * sampleRate

      dataSize = blockAlign * s%points
      riffSize = 4_i4 + 8_i4 + fmtSize + 8_i4 + dataSize

      if (appl) riffSize = riffSize + 8_i4 + applSize

      if (allocated(s%meta)) riffSize = riffSize + 8_i4 + len(s%meta)

      if (file .eq. 'stdout' .or. file .eq. 'http') then
         if (file .eq. 'http') then
            write (*, "('Content-Type: audio/x-wav')")
            write (*, "('Content-Length: ', I0, /)") riffSize + 8
         end if

         write (*, '(*(A))', advance='no') &
            'RIFF', c(riffSize), 'WAVE', &
            'fmt ', c(fmtSize), c(formatTag), c(s%channels), &
            c(sampleRate), c(byteRate), c(blockAlign), c(sampleSize), &
            'data', c(dataSize), c(s%sound)

         if (appl) write (*, '(*(A))', advance='no') &
            'APPL', c(applSize), encode(s%amplitude)

         if (allocated(s%meta)) write (*, '(*(A))', advance='no') &
            'ID3 ', c(len(s%meta, i4)), s%meta
      else
         open (newunit=unit, file=file, &
            action='write', status='replace', access='stream')

         write (unit) 'RIFF', riffSize, 'WAVE', &
            'fmt ', fmtSize, formatTag, s%channels, &
            sampleRate, byteRate, blockAlign, sampleSize, &
            'data', dataSize, s%sound

         if (appl) write (unit) 'APPL', applSize, encode(s%amplitude)

         if (allocated(s%meta)) write (unit) 'ID3 ', len(s%meta, i4), s%meta

         close (unit)
      end if
   end subroutine write_riff
end module riff
