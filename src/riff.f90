module riff
   use bytes, only: c
   use constants, only: audio, dp, eof, i2, i4, stderr
   use extended, only: decode, encode
   implicit none
   private

   public :: read_riff, write_riff

contains

   subroutine read_riff(path, s)
      character(*), intent(in) :: path
      type(audio), intent(out) :: s

      integer :: i, fun, error
      character(1) :: byte
      character(4) :: ckID, formType, applicationSignature
      character(10) :: extended
      integer(i4) :: ckSize, sampleRate, byteRate
      integer(i2) :: sampleSize, formatTag, blockAlign

      open (newunit=fun, file=path, iostat=error, &
         action='read', status='old', access='stream')

      if (error .ne. 0) then
         write (stderr, "('Error: Cannot read RIFF file ''', A, '''.')") path
         stop
      end if

      do
         read (fun, iostat=error) ckID, ckSize
         if (error .eq. eof) exit

         select case (ckID)
         case ('RIFF')
            read (fun) formType

         case ('fmt ')
            read (fun) formatTag, s%channels, sampleRate, byteRate
            read (fun) blockAlign, sampleSize

            s%rate = real(sampleRate, dp)

            if (sampleSize .ne. 16_i2) then
               write (stderr, "('Error: Only 16 bits supported.')")
               stop
            end if

         case ('data')
            s%points = ckSize / (2 * s%channels)
            allocate(s%sound(s%channels, s%points))
            read (fun) s%sound

         case ('APPL')
            read (fun) applicationSignature

            if (applicationSignature .eq. 'FETZ') then
               read (fun) extended
               s%amplitude = decode(extended)
            else
               read (fun) (byte, i = 1, ckSize - 4)
            end if

         case ('ID3 ', 'id3 ')
            allocate(character(ckSize) :: s%meta)
            read (fun) s%meta

         case default
            do i = 1, ckSize
               read (fun, iostat=error) byte

               if (error .ne. 0) then
                  write (stderr, "('Error: Corrupt RIFF file ''', A, '''.')") &
                     path
                  stop
               end if
            end do
         end select
      end do

      close (fun)
   end subroutine read_riff

   subroutine write_riff(path, s)
      character(*), intent(in) :: path
      type(audio), intent(in) :: s

      integer :: fun, error
      integer(i4), parameter :: fmtSize = 16_i4, applSize = 14_i4
      integer(i4) :: riffSize, dataSize, sampleRate, byteRate
      integer(i2), parameter :: sampleSize = 16_i2, formatTag = 1_i2
      integer(i2) :: blockAlign

      blockAlign = 2_i2 * s%channels
      sampleRate = nint(s%rate, i4)
      byteRate = blockAlign * sampleRate

      dataSize = blockAlign * s%points
      riffSize = 4_i4 + 8_i4 + fmtSize + 8_i4 + dataSize

      if (s%amplitude .ne. 1.0_dp) riffSize = riffSize + 8_i4 + applSize

      if (allocated(s%meta)) riffSize = riffSize + 8_i4 + len(s%meta)

      if (path .eq. 'stdout' .or. path .eq. 'http') then
         if (path .eq. 'http') then
            write (*, "('Content-Type: audio/x-wav')")
            write (*, "('Content-Length: ', I0, /)") riffSize + 8
         end if

         write (*, '(*(A))', advance='no') &
            'RIFF', c(riffSize), 'WAVE', &
            'fmt ', c(fmtSize), c(formatTag), c(s%channels), &
            c(sampleRate), c(byteRate), c(blockAlign), c(sampleSize), &
            'data', c(dataSize), c(s%sound)

         if (s%amplitude .ne. 1.0_dp) write (*, '(*(A))', advance='no') &
            'APPL', c(applSize), 'FETZ', encode(s%amplitude)

         if (allocated(s%meta)) write (*, '(*(A))', advance='no') &
            'ID3 ', c(len(s%meta, i4)), s%meta
      else
         open (newunit=fun, file=path, iostat=error, &
            action='write', status='replace', access='stream')

         if (error .ne. 0) then
            write (stderr, "('Error: Cannot write RIFF file ''', A, '''.')") &
               path
            stop
         end if

         write (fun) 'RIFF', riffSize, 'WAVE', &
            'fmt ', fmtSize, formatTag, s%channels, &
            sampleRate, byteRate, blockAlign, sampleSize, &
            'data', dataSize, s%sound

         if (s%amplitude .ne. 1.0_dp) write (fun) &
            'APPL', applSize, 'FETZ', encode(s%amplitude)

         if (allocated(s%meta)) write (fun) &
            'ID3 ', len(s%meta, i4), s%meta

         close (fun)
      end if
   end subroutine write_riff
end module riff
