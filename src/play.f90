subroutine playz
   use aiff, only: read_aiff
   use constants, only: audio
   use interpreter, only: play
   use io, only: command_argument, slurp
   use paths, only: extension
   use riff, only: write_riff
   use tab, only: preprocess
   implicit none

   character(:), allocatable :: infile, outfile
   type(audio) :: s

   infile = command_argument(1, '/dev/stdin')
   outfile = command_argument(2, '/dev/shm/tmp.wav')

   select case (extension(infile))
   case ('wave', 'wav', 'mp3', 'WAVE', 'WAV', 'MP3')
      call execute_command_line('xdg-open ' // infile)
      return

   case ('aiff', 'aif', 'AIFF', 'AIF')
      call read_aiff(infile, s)

   case default
      call play(preprocess(slurp(infile)), s)
   end select

   call write_riff(outfile, s)
   call execute_command_line('xdg-open ' // outfile)
end subroutine playz
