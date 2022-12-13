program tag
   use constants, only: audio, dp
   use id3, only: write_id3
   use io, only: command_argument
   use paths, only: stem
   use riff, only: read_riff, write_riff
   implicit none

   integer :: i
   character(:), allocatable :: infile, datafile, outfile
   type(audio) :: s

   infile = command_argument(1, '/dev/stdin')

   datafile = command_argument(2, '-')

   if (datafile .eq. '-') datafile = stem(infile) // '.id3'

   outfile = command_argument(3, '-')

   if (outfile .eq. '-') then
      i = index(infile, 'stdin', back=.true.)

      if (i .ne. 0) then
         outfile = infile(:i - 1) // 'stdout' // infile(i + 6:)
      else
         outfile = infile
      end if
   end if

   call read_riff(infile, s)

   s%meta = write_id3(datafile)

   if (len(s%meta) .eq. 0) deallocate(s%meta)

   call write_riff(outfile, s)
end program tag
