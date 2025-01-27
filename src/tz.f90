program tz
   use io, only: command_argument
   implicit none

   external :: aiff2riff, guitar, harmonics, inspect, mel, mono, repeatz, &
      riff2aiff, stack, stick, stretch, tag, trimz

   character(:), allocatable :: command

   command = command_argument(0, 'help')

   select case (command)
   case ('mel'); call mel
   case ('guitar'); call guitar
   case ('stick'); call stick
   case ('stack'); call stack
   case ('stretch'); call stretch
   case ('repeat'); call repeatz
   case ('trim'); call trimz
   case ('mono'); call mono
   case ('harmonics'); call harmonics
   case ('inspect'); call inspect
   case ('riff2aiff'); call riff2aiff
   case ('aiff2riff'); call aiff2riff
   case ('tag'); call tag
   case ('help'); call help
   case default
      write (*, "('Unknown command ""', A, '""', /)") command
      call help
   end select

contains

   subroutine help
      write (*, "(A, /, *(:, /, '    tz ', A))") &
         'Tonbandfetzen usage:', &
         'mel [[[wavefile ...] infile] outfile]', &
         'guitar [infile [outfile]]', &
         'stick [[infile ...] outfile]', &
         'stack [[infile ...] outfile]', &
         'stretch [factor [infile [outfile]]]', &
         'repeat [count [infile [outfile]]]', &
         'trim [threshold [infile [outfile]]]', &
         'mono [infile [outfile ...]]', &
         'harmonics [label]', &
         'inspect [file]', &
         'riff2aiff [[infile] outfile]', &
         'aiff2riff [[infile] outfile]', &
         'tag [infile [datafile [outfile]]]'
   end subroutine help
end program tz
