program inspect
   use aiff, only: read_aiff
   use constants, only: audio, stderr
   use io, only: command_argument
   use paths, only: extension
   use riff, only: read_riff
   implicit none

   character(:), allocatable :: path
   type(audio) :: s

   path = command_argument(1, '/dev/stdin')

   select case (extension(path))
   case ('aiff', 'aif')
      call read_aiff(path, s)

   case ('wave', 'wav', '')
      call read_riff(path, s)

   case default
      write (stderr, "('Error: Unknown filename extension.')")
      stop
   end select

   write (*, "('number of channels: ', I0)") s%channels
   write (*, "('number of sample points: ', I0)") s%points
   write (*, "('sample rate: ', F0.1, ' Hz')") s%rate
   write (*, "('duration: ', F0.1, ' s')") s%points / s%rate
   write (*, "('amplitude: ', F0.1)") s%amplitude
end program inspect
