module riff
   use constants
   use extended
   implicit none
   private

   public :: read_riff, write_riff

contains

   subroutine read_riff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(inout) :: s

      integer, parameter :: unit = 14
      integer :: position, error

      integer(i2) :: audio_format, bits, block_align
      integer(i4) :: sample_rate, byte_rate, bytes

      character(4) :: id
      character(10) :: extended

      write (*, "('reading audio file: ', A)") file

      open(unit,                  &
         &    file=file,          &
         &  action='read',        &
         &  status='old',         &
         &    form='unformatted', &
         &  access='stream')

      position = 13

      do
         read (unit, pos=position, iostat=error) id, bytes

         if (error .ne. 0) exit

         position = position + 8

         write (*, "('. reading ', A, ' chunk')") trim(id)

         select case (id)
            case ('fmt ')
               read (unit, pos=position) audio_format, &
                  & s%channels, sample_rate, byte_rate, block_align, bits

               s%rate = real(sample_rate, dp)

               write (*, "('. . number of channels: ', I0)") s%channels
               write (*, "('. . bits per channel and point: ', I0)") bits

               if (bits .ne. 16) then
                  write (*, "('. . . ERROR: only 16 bits supported')")
                  stop
               end if

               write (*, "('. . sample rate: ', F0.1, 'Hz')") s%rate

            case ('data')
               s%points = int(bytes / s%channels / 2, i4)

               write (*, "('. . number of points in time: ', I0)") s%points
               write (*, "('. . duration: ', F0.1, 's')") s%points / s%rate

               if (allocated(s%sound)) deallocate(s%sound)
               allocate(s%sound(s%channels, s%points))

               read (unit, pos=position) s%sound

            case ('INFO')
               read (unit, pos=position) extended

               s%amplitude = decode(extended)

               write (*, "('. . amplitude: ', F0.1)") s%amplitude

            case default
               write (*, "('. . ignored')")
         end select

         position = position + bytes
      end do

      close(unit)
   end subroutine read_riff

   subroutine write_riff(file, s)
      character(*), intent(in) :: file
      type(audio), intent(inout) :: s

      integer, parameter :: unit = 15
      integer(i2), parameter :: audio_format = 1, bits = 16
      integer(i2) :: block_align
      integer(i4) :: sample_rate, byte_rate, size

      block_align = 2_i2 * s%channels
      sample_rate = nint(s%rate, i4)
      byte_rate = block_align * sample_rate
      size = block_align * s%points

      write (*, "('writing audio file: ', A)") file

      open(unit,                  &
         &    file=file,          &
         &  action='write',       &
         &  status='replace',     &
         &    form='unformatted', &
         &  access='stream')

      write (unit) 'RIFF', int(36 + size, 4), 'WAVE'

      write (unit) 'fmt ', 16_i4, audio_format, &
         & s%channels, sample_rate, byte_rate, block_align, bits

      write (unit) 'data', size, s%sound

      if (s%amplitude .ne. 1.0_dp) write (unit) 'INFO', 10_i4, encode(s%amplitude)

      close(unit)
   end subroutine write_riff
end module riff
