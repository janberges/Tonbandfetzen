module aiff
   use bytes, only: c, r
   use constants, only: audio, dp, eof, i2, i4, stderr
   use extended, only: decode, encode
   implicit none
   private

   public :: read_aiff, write_aiff

contains

   subroutine read_aiff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s

      integer :: unit
      integer :: i, error
      character(1) :: byte
      character(4) :: ckID, formType, applicationSignature
      character(10) :: extended
      integer(i4) :: ckSize, offset, blockSize
      integer(i2) :: sampleSize

      open (newunit=unit, file=file, iostat=error, &
         action='read', status='old', access='stream')

      if (error .ne. 0) then
         write (stderr, "('Error: Cannot read AIFF file ''', A, '''.')") file
         stop
      end if

      do
         read (unit, iostat=error) ckID, ckSize
         if (error .eq. eof) exit

         ckSize = r(ckSize)

         select case (ckID)
         case ('FORM')
            read (unit) formType

         case ('COMM')
            read (unit) s%channels, s%points, sampleSize, extended

            s%channels = r(s%channels)
            s%points = r(s%points)
            sampleSize = r(sampleSize)
            s%rate = decode(extended)

            if (sampleSize .ne. 16_i2) then
               write (stderr, "('Error: Only 16 bits supported.')")
               stop
            end if

         case ('SSND')
            allocate(s%sound(s%channels, s%points))
            read (unit) offset, blockSize, s%sound

            offset = r(offset)
            blockSize = r(blockSize)
            s%sound = r(s%sound)

         case ('APPL')
            read (unit) applicationSignature

            if (applicationSignature .eq. 'FETZ') then
               read (unit) extended
               s%amplitude = decode(extended)
            else
               read (unit) (byte, i = 1, ckSize - 4)
            end if

         case ('ID3 ')
            allocate(character(ckSize) :: s%meta)
            read (unit) s%meta

         case default
            do i = 1, ckSize
               read (unit, iostat=error) byte

               if (error .ne. 0) then
                  write (stderr, "('Error: Corrupt AIFF file ''', A, '''.')") &
                     file
                  stop
               end if
            end do
         end select
      end do

      close (unit)
   end subroutine read_aiff

   subroutine write_aiff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(in) :: s

      integer :: unit, error
      integer(i4), parameter :: commSize = 18_i4, applSize = 14_i4
      integer(i4), parameter :: offset = 0_i4, blockSize = 0_i4
      integer(i4) :: formSize, ssndSize
      integer(i2), parameter :: sampleSize = 16_i2
      integer(i2) :: blockAlign

      blockAlign = 2_i2 * s%channels

      ssndSize = 8_i4 + blockAlign * s%points
      formSize = 4_i4 + 8_i4 + commSize + 8_i4 + ssndSize

      if (s%amplitude .ne. 1.0_dp) formSize = formSize + 8_i4 + applSize

      if (allocated(s%meta)) formSize = formSize + 8_i4 + len(s%meta)

      if (file .eq. 'stdout' .or. file .eq. 'http') then
         if (file .eq. 'http') then
            write (*, "('Content-Type: audio/x-aiff')")
            write (*, "('Content-Length: ', I0, /)") formSize + 8
         end if

         write (*, '(*(A))', advance='no') &
            'FORM', c(r(formSize)), 'AIFF', &
            'COMM', c(r(commSize)), c(r(s%channels)), &
            c(r(s%points)), c(r(sampleSize)), encode(s%rate), &
            'SSND', c(r(ssndSize)), c(r(offset)), c(r(blockSize)), &
            c(r(s%sound))

         if (s%amplitude .ne. 1.0_dp) write (*, '(*(A))', advance='no') &
            'APPL', c(r(applSize)), 'FETZ',  encode(s%amplitude)

         if (allocated(s%meta)) write (*, '(*(A))', advance='no') &
            'ID3 ', c(r(len(s%meta, i4))), s%meta
      else
         open (newunit=unit, file=file, iostat=error, &
            action='write', status='replace', access='stream')

         if (error .ne. 0) then
            write (stderr, "('Error: Cannot write AIFF file ''', A, '''.')") &
               file
            stop
         end if

         write (unit) 'FORM', r(formSize), 'AIFF', &
            'COMM', r(commSize), r(s%channels), &
            r(s%points), r(sampleSize), encode(s%rate), &
            'SSND', r(ssndSize), r(offset), r(blockSize), &
            r(s%sound)

         if (s%amplitude .ne. 1.0_dp) write (unit) &
            'APPL', r(applSize), 'FETZ', encode(s%amplitude)

         if (allocated(s%meta)) write (unit) &
            'ID3 ', r(len(s%meta, i4)), s%meta

         close (unit)
      end if
   end subroutine write_aiff
end module aiff
