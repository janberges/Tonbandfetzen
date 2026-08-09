subroutine playz
   use aiff, only: read_aiff
   use constants, only: audio
   use interpreter, only: play
   use io, only: command_argument, slurp
   use paths, only: extension
   use riff, only: write_riff
   use tab, only: preprocess
   implicit none

   character(:), allocatable :: infile, outfile, command
   type(audio) :: s
   logical :: macos

   inquire (file='/usr/bin/sw_vers', exist=macos)

   if (macos) then
      outfile = 'tz-play.tmp.wav'
      command = 'open -a "QuickTime Player"'
   else
      outfile = '/dev/shm/tz-play.tmp.wav'
      command = 'xdg-open'
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
