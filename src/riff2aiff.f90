program riff2aiff
   use aiff
   use constants
   use io
   use riff
   implicit none

   character(:), allocatable :: aif, wav
   type(audio) :: s

   wav = command_argument(1)
   aif = command_argument(2)

   call read_riff(wav, s)
   call write_aiff(aif, s)
end program riff2aiff
