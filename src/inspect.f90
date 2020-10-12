program inspect
   use aiff
   use constants
   use io
   use riff
   implicit none

   character(:), allocatable :: path
   type(audio) :: s

   path = command_argument(1)

   if (path(len(path) - 3:) .eq. '.aif') then
      call read_aiff(path, s)
   else
      call read_riff(path, s)
   end if

   write (*, "('number of channels: ', I0)") s%channels
   write (*, "('number of sample points: ', I0)") s%points
   write (*, "('sample rate: ', F0.1, 'Hz')") s%rate
   write (*, "('duration: ', F0.1, 's')") s%points / s%rate
   write (*, "('amplitude: ', F0.1)") s%amplitude
end program inspect
