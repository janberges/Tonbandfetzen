program aiff2riff
   use aiff
   use constants
   use io
   use riff
   implicit none

   character(:), allocatable :: aif, wav
   type(audio) :: s

   aif = command_argument(1)
   wav = command_argument(2)

   call read_aiff(aif, s)
   call write_riff(wav, s)
end program aiff2riff
