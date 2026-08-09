subroutine playz
   use aiff, only: read_aiff
   use constants, only: audio
   use interpreter, only: play
   use io, only: command_argument, environment_variable, slurp
   use paths, only: extension
   use riff, only: write_riff
   use tab, only: preprocess
   implicit none

   character(:), allocatable :: infile, outfile, command
   type(audio) :: s

   outfile = 'tz-play.tmp.wav'
   command = 'open'

   if (index(environment_variable('OSTYPE'), 'darwin') .eq. 0) then
      outfile = '/dev/shm/' // outfile
      command = 'xdg-' // command
   end if

   infile = command_argument(1, '/dev/stdin')
   outfile = command_argument(2, outfile)

   select case (extension(infile))
   case ('wave', 'wav', 'mp3', 'WAVE', 'WAV', 'MP3')
      call execute_command_line(command // ' ' // infile)
      return

   case ('aiff', 'aif', 'AIFF', 'AIF')
      call read_aiff(infile, s)

   case default
      call play(preprocess(slurp(infile)), s)
   end select

   call write_riff(outfile, s)
   call execute_command_line(command // ' ' // outfile)
end subroutine playz
