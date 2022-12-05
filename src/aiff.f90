module aiff
   use bytes, only: c, r
   use constants, only: audio, dp, eof, i2, i4, stderr
   use extended, only: decode, encode
   use id3, only: read_id3, write_id3
   implicit none
   private

   public :: read_aiff, write_aiff

contains

   subroutine read_aiff(file, s, id3)
      character(*), intent(in) :: file
      type(audio), intent(out) :: s
      logical, optional :: id3

      integer :: unit
      integer :: i, error
      character(1) :: byte
      character(4) :: ckID, formType
      character(10) :: extended
      integer(i4) :: ckSize, offset, blockSize
      integer(i2) :: sampleSize

      open (newunit=unit, file=file, &
         action='read', status='old', access='stream')

      do
         read (unit, iostat=error) ckID, ckSize
         if (error .eq. eof) exit

         ckSize = r(ckSize)

         if (present(id3)) then
            if (id3 .and. ckID .eq. 'ID3' .or. ckID .eq. 'id3') ckID = 'ID3?'
         end if

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
            read (unit) extended
            s%amplitude = decode(extended)

         case ('ID3?')
            call read_id3(unit)

         case default
            read (unit) (byte, i = 1, ckSize)
         end select
      end do

      close (unit)
   end subroutine read_aiff

   subroutine write_aiff(filex, s)
      character(*), intent(in) :: filex
      type(audio), intent(in) :: s

      integer :: unit
      logical :: appl
      character(:), allocatable :: file, id3
      integer(i4), parameter :: commSize = 18_i4, applSize = 10_i4
      integer(i4), parameter :: offset = 0_i4, blockSize = 0_i4
      integer(i4) :: formSize, ssndSize
      integer(i2), parameter :: sampleSize = 16_i2
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

      ssndSize = 8_i4 + blockAlign * s%points
      formSize = 4_i4 + 8_i4 + commSize + 8_i4 + ssndSize

      if (appl) formSize = formSize + 8_i4 + applSize

      if (len(id3) .gt. 0) formSize = formSize + 8_i4 + len(id3)

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

         if (appl) write (*, '(*(A))', advance='no') &
            'APPL', c(r(applSize)), encode(s%amplitude)

         if (len(id3) .gt. 0) write (*, '(*(A))', advance='no') &
            'ID3 ', c(r(len(id3, i4))), id3
      else
         open (newunit=unit, file=file, &
            action='write', status='replace', access='stream')

         write (unit) 'FORM', r(formSize), 'AIFF', &
            'COMM', r(commSize), r(s%channels), &
            r(s%points), r(sampleSize), encode(s%rate), &
            'SSND', r(ssndSize), r(offset), r(blockSize), &
            r(s%sound)

         if (appl) write (unit) 'APPL', r(applSize), encode(s%amplitude)

         if (len(id3) .gt. 0) write (unit) 'ID3 ', r(len(id3, i4)), id3

         close (unit)
      end if
   end subroutine write_aiff
end module aiff
