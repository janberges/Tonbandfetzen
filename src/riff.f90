module riff
   use bytes, only: c
   use constants, only: audio, dp, eof, i2, i4, stderr
   use extended, only: decode, encode
   use id3, only: read_id3, write_id3
   implicit none
   private

   public :: read_riff, write_riff

contains

   subroutine read_riff(file, s, id3)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s
      logical, optional :: id3

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

         if (present(id3)) then
            if (id3 .and. ckID .eq. 'ID3' .or. ckID .eq. 'id3') ckID = 'ID3?'
         end if

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

         case ('ID3?')
            call read_id3(unit)

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
      character(:), allocatable :: file, id3
      integer(i4), parameter :: fmtSize = 16_i4, applSize = 10_i4
      integer(i4) :: riffSize, dataSize, sampleRate, byteRate
      integer(i2), parameter :: sampleSize = 16_i2, formatTag = 1_i2
      integer(i2) :: blockAlign

      if (filex(len(filex):len(filex)) .eq. '/') then
         file = filex(:len(filex) - 1)
         appl = .false.
         id3 = write_id3(file)
      else
         file = filex
         appl = s%amplitude .ne. 1.0_dp
         id3 = ''
      end if

      blockAlign = 2_i2 * s%channels
      sampleRate = nint(s%rate, i4)
      byteRate = blockAlign * sampleRate

      dataSize = blockAlign * s%points
      riffSize = 4_i4 + 8_i4 + fmtSize + 8_i4 + dataSize

      if (appl) riffSize = riffSize + 8_i4 + applSize

      if (len(id3) .gt. 0) riffSize = riffSize + 8_i4 + len(id3)

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

         if (len(id3) .gt. 0) write (*, '(*(A))', advance='no') &
            'ID3 ', c(len(id3, i4)), id3
      else
         open (newunit=unit, file=file, &
            action='write', status='replace', access='stream')

         write (unit) 'RIFF', riffSize, 'WAVE', &
            'fmt ', fmtSize, formatTag, s%channels, &
            sampleRate, byteRate, blockAlign, sampleSize, &
            'data', dataSize, s%sound

         if (appl) write (unit) 'APPL', applSize, encode(s%amplitude)

         if (len(id3) .gt. 0) write (unit) 'ID3 ', len(id3, i4), id3

         close (unit)
      end if
   end subroutine write_riff
end module riff
