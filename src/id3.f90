module id3
   use constants, only: dp, eof, i1, stderr
   use io, only: slurp
   use paths, only: extension
   implicit none
   private

   public :: read_id3, write_id3

contains

   subroutine read_id3(id3)
      character(*), intent(in) :: id3

      integer :: i, n, flags, tagSize, frameSize
      integer(i1) :: version, revision
      character(3) :: tagID
      character(4) :: frameID
      character(:), allocatable :: feature, text

      tagID = id3(1:3)

      if (tagID .eq. 'TAG') then
         write (stderr, "('Warning: ID3v1 not supported.')") feature
         return
      end if

      version = ichar(id3(4:4), i1)
      revision = ichar(id3(5:5), i1)

      if (version .le. 2) then
         write (stderr, "('Warning: ', A, 'v2.', I0, ' not supported.')") &
            tagID, version
         return
      end if

      write (stderr, "('Metadata format: ', A, 'v2.', I0, '.', I0)") &
         tagID, version, revision

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

      tagSize = decode_size(id3(7:10))

      do while (i .lt. 10 + tagSize)
         if (ichar(id3(i + 1:i + 1)) .eq. 0) exit

         frameID = id3(i + 1:i + 4)

         select case (frameID)
         case ('APIC')
            feature = 'picture'
         case ('COMM')
            feature = 'comment'
         case ('PRIV')
            feature = 'private'
         case ('TALB')
            feature = 'album'
         case ('TCOM')
            feature = 'composer'
         case ('TCON')
            feature = 'genre'
         case ('TCOP')
            feature = 'copyright'
         case ('TENC')
            feature = 'encoded by'
         case ('TIT2')
            feature = 'title'
         case ('TLEN')
            feature = 'duration/ms'
         case ('TOPE')
            feature = 'original artist'
         case ('TPE1')
            feature = 'artist'
         case ('TPE2')
            feature = 'album artist'
         case ('TPOS')
            feature = 'disc number'
         case ('TRCK')
            feature = 'track number'
         case ('TYER')
            feature = 'year'
         case ('WAAA':'WXXX')
            feature = 'website'
         case default
            feature = 'unknown'
         end select

         frameSize = decode_size(id3(i + 5:i + 8), synchsafe=version .eq. 4_i1)

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
            text = id3(i + 2:i + frameSize)

            select case (ichar(id3(i + 1:i + 1)))
            case (0) ! ISO-8859-1
               text = encode_utf8(decode_iso8859_1(text))
            case (1, 2) ! UTF-16, UTF-16BE
               text = encode_utf8(decode_utf16(text))
            case (3) ! UTF-8
               continue
            end select

            do
               n = index(text, char(0))

               if (n .eq. 0) exit

               if (n .eq. len(text)) then
                  text = text(:n - 1)
               else
                  text = text(:n - 1) // '/' // text(n + 1:)
               end if
            end do
         else
            text = repeat(' ', 64)
            write (text, "(I0, ' bytes')") frameSize
            text = trim(text)
         end if

         if (feature .ne. 'unknown') then
            write (stderr, "(A, ' (', A, '): ', A)") frameID, feature, text
         else
            write (stderr, "(A, ': ', A)") frameID, text
         end if

         i = i + frameSize
      end do
   end subroutine read_id3

   function write_id3(file) result(id3)
      character(:), allocatable :: id3

      character(*), intent(in) :: file

      character(4) :: frameID
      character(256) :: buffer
      character(:), allocatable :: text, mime

      integer(i1), parameter :: version = 4_i1
      integer(i1), parameter :: revision = 0_i1
      integer(i1), parameter :: flags = 0_i1
      integer(i1), parameter :: encoding = 3_i1

      integer :: unit, error
      logical :: exist

      inquire (file=file, exist=exist)

      id3 = ''

      if (exist) then
         open (newunit=unit, file=file, action='read', status='old')

         do
            read (unit, '(A4, 1X, A)', iostat=error) frameID, buffer
            text = trim(buffer)

            if (error .eq. eof) exit

            if (frameID .eq. 'APIC') then
               select case(extension(text))
               case ('jpeg', 'jpg', 'JPEG', 'JPG')
                  mime = 'image/jpeg'
               case ('png', 'PNG')
                  mime = 'image/png'
               case ('svg', 'SVG')
                  mime = 'image/svg+xml'
               case ('tiff', 'tif', 'TIFF', 'TIF')
                  mime = 'image/tiff'
               case ('gif', 'GIF')
                  mime = 'image/gif'
               case ('bmp', 'BMP')
                  mime = 'image/bmp'
               end select

               text = mime // char(0) // char(3) // 'cover' // char(0) &
                  // slurp(text)
            else if (frameID(1:1) .ne. 'T') then
               write (stderr, "('Warning: ', A, ' not supported.')") frameID
               continue
            end if

            id3 = id3 // frameID // encode_size(1 + len(text), &
               synchsafe=version .eq. 4_i1) // char(flags) // char(flags) &
               // char(encoding) // text
         end do

         close (unit)
      end if

      if (len(id3) .gt. 0) id3 = 'ID3' // char(version) // char(revision) &
         // char(flags) // encode_size(len(id3)) // id3
   end function write_id3

   function decode_size(code, synchsafe) result(value)
      integer :: value

      character(*), intent(in) :: code
      logical, intent(in), optional :: synchsafe

      integer :: byte, base

      base = 128

      if (present(synchsafe)) then
         if (.not. synchsafe) base = 256
      end if

      value = 0
      do byte = 1, 4
         value = value + ichar(code(byte:byte)) * base ** (4 - byte)
      end do
   end function decode_size

   function encode_size(value, synchsafe) result(code)
      character(4) :: code

      integer, intent(in) :: value
      logical, intent(in), optional :: synchsafe

      integer :: byte, base

      base = 128

      if (present(synchsafe)) then
         if (.not. synchsafe) base = 256
      end if

      do byte = 1, 4
         code(byte:byte) = char(modulo(value / base ** (4 - byte), base))
      end do
   end function encode_size

   function decode_iso8859_1(code) result(value)
      integer, allocatable :: value(:)

      character(*), intent(in) :: code

      integer :: byte

      allocate(value(len(code)))

      do byte = 1, len(code)
         value(byte) = ichar(code(byte:byte))
      end do
   end function decode_iso8859_1

   function decode_utf16(code) result(value)
      integer, allocatable :: value(:)

      character(*), intent(in) :: code

      integer :: c, s, v
      logical :: be

      allocate(value(len(code) / 2))

      be = .true.

      v = 1
      do c = 1, len(code), 2
         if (be) then
            s = ichar(code(c:c)) * 256 + ichar(code(c + 1:c + 1))
         else
            s = ichar(code(c + 1:c + 1)) * 256 + ichar(code(c:c))
         end if

         if (s .eq. 65279) then ! FEFF (BOM)
            continue
         else if (s .eq. 65534) then ! FFFE (wrong BOM)
            be = .not. be
         else if (55296 .le. s .and. s .lt. 56320) then ! high surrogate
            value(v) = 1024 * (s - 55296)
         else if (56320 .le. s .and. s .lt. 57344) then ! low surrogate
            value(v) = 65536 + s - 56320 + value(v)
            v = v + 1
         else ! basic multilingual plane
            value(v) = s
            v = v + 1
         end if
      end do

      value = value(:v - 1)
   end function decode_utf16

   function encode_utf8(value) result(code)
      character(:), allocatable :: code

      integer, intent(in) :: value(:)

      integer :: c, i, j, n, v

      code = ''

      do v = lbound(value, 1), ubound(value, 1)
         if (value(v) .lt. 128) then
            code = code // char(value(v))
         else
            do n = 1, 5
               if (value(v) .lt. 64 * 32 ** n) then
                  do i = n, 0, -1
                     c = 128 + modulo(value(v) / 64 ** i, 64)
                     if (i .eq. n) then
                        do j = 1, n
                           c = c + 128 / 2 ** j
                        end do
                     end if
                     code = code // char(c)
                  end do
                  exit
               end if
            end do
         end if
      end do
   end function encode_utf8
end module id3
