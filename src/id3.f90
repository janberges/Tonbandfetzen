module id3
   use constants, only: dp, eof, i1, stderr
   use paths, only: stem
   implicit none
   private

   public :: read_id3, write_id3

   integer, parameter :: int7max = 2 ** 7

contains

   subroutine read_id3(id3)
      character(*), intent(in) :: id3

      integer :: i, flags, tagSize, frameSize, first
      character(4) :: frameID
      character(:), allocatable :: feature

      write (stderr, "('Metadata format: ', A, 'v2.', I0, '.', I0)") &
         id3(1:3), ichar(id3(4:4)), ichar(id3(5:5))

      flags = ichar(id3(6:6))

      do i = 7, 4, -1
         if (btest(flags, i)) then
            select case (i)
            case (7)
               feature = 'unsynchronisation'
            case (6)
               feature = 'extended header'
            case (5)
               feature = 'experimental tag'
            case (4)
               feature = 'footer'
            end select

            write (stderr, "('Warning: ID3 ', A, ' not supported.')") feature
            return
         end if
      end do

      i = 10

      tagSize = decode_synchsafe(id3(7:10))

      do while (i .lt. 10 + tagSize)
         if (ichar(id3(i + 1:i + 1)) .eq. 0) exit

         frameID = id3(i + 1:i + 4)

         frameSize = decode_synchsafe(id3(i + 5:i + 8))

         flags = ichar(id3(i + 9:i + 9))

         if (btest(flags, 6)) write (stderr, "(A, ' tag-bound')") frameID
         if (btest(flags, 5)) write (stderr, "(A, ' file-bound')") frameID
         if (btest(flags, 4)) write (stderr, "(A, ' read-only')") frameID

         flags = ichar(id3(i + 10:i + 10))

         if (btest(flags, 6)) write (stderr, "(A, ' grouped')") frameID
         if (btest(flags, 3)) write (stderr, "(A, ' compressed')") frameID
         if (btest(flags, 2)) write (stderr, "(A, ' encrypted')") frameID
         if (btest(flags, 1)) write (stderr, "(A, ' unsyrchronised')") frameID
         if (btest(flags, 0)) write (stderr, "(A, ' states length')") frameID

         i = i + 10

         if (frameID(1:1) .eq. 'T') then
            select case (frameID)
            case ('TIT2')
               feature = 'Title'
            case ('TALB')
               feature = 'Album'
            case ('TPE1')
               feature = 'Artist'
            case ('TYER')
               feature = 'Year'
            end select

            select case (ichar(id3(i + 1:i + 1)))
            case (0, 3) ! ISO-8859-1, UTF-8
               first = i + 3
            case (1, 2) ! UTF-16, UTF-16BE
               first = i + 4
            end select

            write (stderr, "(A, ': ', A)") feature, id3(first:i + frameSize)
         end if

         i = i + frameSize
      end do
   end subroutine read_id3

   function write_id3(file) result(id3)
      character(:), allocatable :: id3

      character(*), intent(in) :: file

      character(:), allocatable :: meta

      character(4) :: frameID
      character(256) :: text

      character(1), parameter :: version = char(4)
      character(1), parameter :: revision = char(0)
      character(1), parameter :: flags = char(0)
      character(1), parameter :: encoding = char(3)

      integer :: unit, error
      logical :: exist

      meta = stem(file) // '.meta'

      inquire (file=meta, exist=exist)

      id3 = ''

      if (exist) then
         open (newunit=unit, file=meta, action='read', status='old')

         do
            read (unit, '(A4, 1X, A)', iostat=error) frameID, text

            if (error .eq. eof) exit

            id3 = id3 // frameID // encode_synchsafe(2 + len(trim(text))) &
               // flags // flags // encoding // flags // trim(text)
         end do

         close (unit)
      end if

      if (len(id3) .gt. 0) id3 = 'ID3' // version // revision &
         // flags // encode_synchsafe(len(id3)) // id3
   end function write_id3

   function decode_synchsafe(code) result(value)
      integer :: value

      character(*), intent(in) :: code

      integer :: byte

      value = 0
      do byte = 1, 4
         value = value + ichar(code(byte:byte)) * int7max  ** (4 - byte)
      end do
   end function decode_synchsafe

   function encode_synchsafe(value) result(code)
      character(4) :: code

      integer, intent(in) :: value

      integer :: byte

      do byte = 1, 4
         code(byte:byte) = char(modulo(value / int7max ** (4 - byte), int7max))
      end do
   end function encode_synchsafe
end module id3
