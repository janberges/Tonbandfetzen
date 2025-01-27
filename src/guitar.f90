subroutine guitar
   use constants, only: stderr
   use io, only: command_argument, slurp
   use tab, only: preprocess
   implicit none

   integer :: unit, error

   character(:), allocatable :: file, notes

   notes = preprocess(slurp(command_argument(1, '/dev/stdin')))

   file = command_argument(2, '/dev/stdout')

   if (file .eq. 'stdout') then
      write (*, '(A)', advance='no') notes
   else
      open (newunit=unit, file=file, iostat=error, &
         action='write', status='replace', access='stream')

      if (error .ne. 0) then
         write (stderr, "('Error: Cannot write file ''', A, '''.')") file
         stop
      end if

      write (unit) notes
      close (unit)
   end if
end subroutine guitar
