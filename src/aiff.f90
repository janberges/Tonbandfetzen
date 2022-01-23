module aiff
   use bytes
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
      integer :: i, error
      integer(i4) :: ckSize, offset, blockSize
      integer(i2) :: sampleSize

      open(unit, file=file, action='read', status='old', &
         form='unformatted', access='stream')

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
            read (unit) extended
            s%amplitude = decode(extended)

         case default
            read (unit) (byte, i = 1, ckSize)
         end select
      end do

      close(unit)
   end subroutine read_aiff

   subroutine write_aiff(filex, s)
      character(*), intent(in) :: filex
      type(audio), intent(in) :: s

      integer, parameter :: unit = 15
      logical :: appl
      character(:), allocatable :: file
      integer(i4), parameter :: commSize = 18_i4, applSize = 10_i4
      integer(i4), parameter :: offset = 0_i4, blockSize = 0_i4
      integer(i4) :: formSize, ssndSize
      integer(i2), parameter :: sampleSize = 16_i2
      integer(i2) :: blockAlign

      if (filex(len(filex):len(filex)) .eq. '/') then
         file = filex(:len(filex) - 1)
         appl = .false.
      else
         file = filex
         appl = s%amplitude .ne. 1.0_dp
      end if

      blockAlign = 2_i2 * s%channels

      ssndSize = 8_i4 + blockAlign * s%points
      formSize = 4_i4 + 8_i4 + commSize + 8_i4 + ssndSize

      if (appl) formSize = formSize + 8_i4 + applSize

      if (file .eq. 'stdout' .or. file .eq. 'http') then
         if (file .eq. 'http') then
            write (stdout, "('Content-Type: audio/x-aiff')")
            write (stdout, "('Content-Length: ', I0, /)") formSize + 8
         end if

         write (stdout, '(*(A))', advance='no') &
            'FORM', c(r(formSize)), 'AIFF', &
            'COMM', c(r(commSize)), c(r(s%channels)), &
            c(r(s%points)), c(r(sampleSize)), encode(s%rate), &
            'SSND', c(r(ssndSize)), c(r(offset)), c(r(blockSize)), &
            c(r(s%sound))

         if (appl) write (stdout, '(*(A))', advance='no') &
            'APPL', c(r(applSize)), encode(s%amplitude)
      else
         open(unit, file=file, action='write', status='replace', &
            form='unformatted', access='stream')

         write (unit) 'FORM', r(formSize), 'AIFF', &
            'COMM', r(commSize), r(s%channels), &
            r(s%points), r(sampleSize), encode(s%rate), &
            'SSND', r(ssndSize), r(offset), r(blockSize), &
            r(s%sound)

         if (appl) write (unit) 'APPL', r(applSize), encode(s%amplitude)

         close(unit)
      end if
   end subroutine write_aiff
end module aiff
