program inspect
   use aiff, only: read_aiff
   use constants, only: audio, stderr
   use id3, only: read_id3
   use io, only: command_argument, slurp
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

   case ('mp3')
      call read_id3(slurp(path))
      stop

   case default
      write (stderr, "('Error: Unknown filename extension.')")
      stop
   end select

   if (allocated(s%meta)) call read_id3(s%meta)

   write (*, "('Number of channels: ', I0)") s%channels
   write (*, "('Number of sample points: ', I0)") s%points
   write (*, "('Sample rate: ', F0.1, ' Hz')") s%rate
   write (*, "('Duration: ', F0.1, ' s')") s%points / s%rate
   write (*, "('Amplitude: ', F0.1)") s%amplitude
end program inspect
