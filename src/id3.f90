module id3
   use constants, only: dp, eof, i1, stderr
   use paths, only: stem
   implicit none
   private

   public :: read_id3, write_id3

   integer, parameter :: int7max = 2 ** 7
   character(*), parameter :: NADA = transfer(0, 'NULL')

contains

   subroutine read_id3(unit)
      integer, intent(in) :: unit

      integer :: i, j, error
      character(1) :: byte
      character(3) :: tagID
      character(4) :: frameID, long
      integer(i1) :: version, revision, flags, encoding
      integer :: tagSize, frameSize, readSize, dataSize
      character(:), allocatable :: feature, title, album, artist, year

      read (unit) tagID, version, revision, flags, long

      tagSize = decode_synchsafe(long)

      write (stderr, "('Metadata format: ID3v2.', I0, '.', I0)") version, revision

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
            read (unit) (byte, j = 1, tagSize)
            return
         end if
      end do

      readSize = 0

      do while (readSize .lt. tagSize)
         read (unit, iostat=error) frameID, long

         if (error .eq. eof) then
            write (stderr, "('Warning: Unexpected end of ID3 tag.')")
            exit
         end if

         frameSize = decode_synchsafe(long)

         read (unit) flags

         if (btest(flags, 6)) write (stderr, "(A, ' tag-bound')") frameID
         if (btest(flags, 5)) write (stderr, "(A, ' file-bound')") frameID
         if (btest(flags, 4)) write (stderr, "(A, ' read-only')") frameID

         read (unit) flags

         if (btest(flags, 6)) write (stderr, "(A, ' grouped')") frameID
         if (btest(flags, 3)) write (stderr, "(A, ' compressed')") frameID
         if (btest(flags, 2)) write (stderr, "(A, ' encrypted')") frameID
         if (btest(flags, 1)) write (stderr, "(A, ' unsyrchronised')") frameID
         if (btest(flags, 0)) write (stderr, "(A, ' states length')") frameID

         readSize = readSize + 10

         dataSize = frameSize

         if (frameID(1:1) .eq. 'T') then
            read (unit) encoding
            dataSize = dataSize - 1

            select case (encoding)
               case (0, 3) ! ISO-8859-1, UTF-8
                  read (unit) byte
                  dataSize = dataSize - 1

               case (1, 2) ! UTF-16, UTF-16BE
                  read (unit) byte, byte
                  dataSize = dataSize - 2
            end select
         end if

         readSize = readSize + frameSize - dataSize

         select case (frameID)
         case ('TIT2')
            allocate(character(dataSize) :: title)
            read (unit) title
            write (stderr, "('Title: ', A)") title

         case ('TALB')
            allocate(character(dataSize) :: album)
            read (unit) album
            write (stderr, "('Album: ', A)") album

         case ('TPE1')
            allocate(character(dataSize) :: artist)
            read (unit) artist
            write (stderr, "('Artist: ', A)") artist

         case ('TYER')
            allocate(character(dataSize) :: year)
            read (unit) year
            write (stderr, "('Year: ', A)") year

         case default
            read (unit) (byte, i = 1, dataSize)

         case (NADA)
            dataSize = tagSize - readSize
            read (unit) (byte, i = 1, dataSize)
         end select

         readSize = readSize + dataSize
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
