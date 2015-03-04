module aiff
   use accuracy
   use extended
   implicit none
   private

   public :: audio, take, make

   type audio
      integer(2) :: channels
      integer(4) :: points

      real(dp) :: rate

      integer(2), allocatable :: sound(:, :)
   end type audio

contains

   subroutine take(file, s)
      character(*), intent(in) :: file
      type(audio), intent(inout) :: s

      integer, parameter :: unit = 14
      integer :: position, error

      integer(2) :: bits
      integer(4) :: bytes

      character(4) :: id
      character(10) :: extended

      write (*, "('reading audio file: ', A)") file

      open(unit,                  &
         &    file=file,          &
         &  action='read',        &
         &  status='old',         &
         &    form='unformatted', &
         &  access='stream',      &
         & convert='big_endian')

      position = 13

      do
         read (unit, pos=position, iostat=error) id, bytes

         if (error .ne. 0) exit

         position = position + 8

         write (*, "('. reading ', A, ' chunk')") trim(id)

         select case (id)
            case ('COMM')
               read (unit, pos=position) s%channels, s%points, bits, extended

               s%rate = decode(extended)

               write (*, "('. . number of channels: ', I0)") s%channels
               write (*, "('. . number of points in time: ', I0)") s%points
               write (*, "('. . bits per channel and point: ', I0)") bits

               if (bits .ne. 16) then
                  write (*, "('. . . ERROR: only 16 bits supported')")
                  stop
               end if

               write (*, "('. . sample rate: ', F0.1, 'Hz')") s%rate
               write (*, "('. . duration: ', F0.1, 's')") s%points / s%rate

            case ('SSND')
               if (allocated(s%sound)) deallocate(s%sound)
               allocate(s%sound(s%channels, s%points))

               read (unit, pos=position + 8) s%sound

            case default
               write (*, "('. . ignored')")
         end select

         position = position + bytes
      end do

      close(unit)
   end subroutine take

   subroutine make(file, s)
      character(*), intent(in) :: file
      type(audio), intent(inout) :: s

      integer, parameter :: unit = 15
      integer :: size

      size = 4 * s%channels * s%points

      write (*, "('writing audio file: ', A)") file

      open(unit,                  &
         &    file=file,          &
         &  action='write',       &
         &  status='replace',     &
         &    form='unformatted', &
         &  access='stream',      &
         & convert='big_endian')

      write (unit) 'FORM', int(76 + size, 4), 'AIFF'

      write (unit) 'COMM', 18_4, s%channels, s%points, 16_2, encode(s%rate)

      write (unit) 'SSND', int(8 + size, 4), 0_4, 0_4, s%sound

      close(unit)
   end subroutine make
end module aiff
