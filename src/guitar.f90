program guitar
   use constants, only: eof, stderr, stdin, stdout
   use io, only: command_argument
   implicit none

   integer :: iu, ou, error, from, till, beats
   logical :: first

   character(:), allocatable :: gtr, mel, line, bar, bars
   character(1048576) :: tmp

   gtr = command_argument(1, '/dev/stdin')
   mel = command_argument(2, '/dev/stdout')

   if (gtr .eq. 'stdin') then
      iu = stdin
   else
      open (newunit=iu, file=gtr, action='read', status='old', iostat=error)

      if (error .ne. 0) then
         write (stderr, "('Error: Cannot read ASCII file ''', A, '''.')") gtr
         stop
      end if
   end if

   if (mel .eq. 'stdout') then
      ou = stdout
   else
      open (newunit=ou, file=mel, action='write', status='replace', &
         iostat=error)

      if (error .ne. 0) then
         write (stderr, "('Error: Cannot write ASCII file ''', A, '''.')") mel
         stop
      end if
   end if

   first = .true.

   do
      read (iu, '(A)', iostat=error) tmp
      if (error .eq. eof) exit

      line = trim(tmp)

      if (matches(line, '|') .gt. 1) then
         if (first) then
            bars = 'M0'
            first = .false.
         else
            bars = 'W0'
         end if

         from = 1
         do while (from .le. len(line))
            till = scan(line(from:), '|')

            if (till .eq. 0) then
               till = len(line)
            else
               till = from + till - 2
            end if

            bar = line(from:till)

            from = till + 2

            if (scan(bar, '-~') .ne. 0) then
               bar = sub(bar, 'X', replace='-')
               bar = sub(bar, 'SZNT', replace='~')

               bar = sub(bar, '-~', invert=.true., &
                  keep=.true., replace='~')

               bar = sub(bar, '0.123456789:', before='-~', &
                  insert='U', keep=.true.)

               beats = matches(bar, '-~')

               bar = sub(bar, '-', insert='"', ratio=.true.)
               bar = sub(bar, '~', insert="'", ratio=.true.)
            end if

            bars = bars // strip(bar)
         end do

         write (ou, '(A)') bars
      else
         if (len(strip(line)) .eq. 0) first = .true.

         write (ou, '(A)') line
      end if
   end do

   if (gtr .ne. 'stdin') close (iu)
   if (mel .ne. 'stdout') close (ou)

contains

   function strip(string)
      character(:), allocatable :: strip
      character(*), intent(in) :: string

      character(*), parameter :: whitespace = char(9) // ' '
      integer :: lower, upper

      lower = verify(string, whitespace)

      if (lower .eq. 0) then
         strip = ''
         return
      end if

      upper = verify(string, whitespace, back=.true.)

      strip = string(lower:upper)
   end function strip

   function matches(string, set)
      integer :: matches
      character(*), intent(in) :: string, set

      integer :: i

      matches = 0

      do i = 1, len_trim(string)
         if (index(set, string(i:i)) .ne. 0) matches = matches + 1
      end do
   end function matches

   function sub(string, set, invert, before, insert, ratio, keep, replace)
      character(:), allocatable :: sub
      character(*), intent(in) :: string, set
      character(*), intent(in), optional :: before, insert, replace
      logical, intent(in), optional :: invert, ratio, keep

      integer :: i, j
      logical :: inverted

      if (present(invert)) then
         inverted = invert
      else
         inverted = .false.
      end if

      sub = ''

      i = 1
      do
         if (i .gt. len(string)) exit

         if (inverted) then
            j = verify(string(i:), set)
         else
            j = scan(string(i:), set)
         end if

         if (j .eq. 0) then
            j = len(string) + 1
         else
            j = i + j - 1
         end if

         sub = sub // string(i:j - 1)

         if (j .gt. len(string)) exit

         if (inverted) then
            i = scan(string(j:), set)
         else
            i = verify(string(j:), set)
         end if

         if (i .eq. 0) then
            i = len(string) + 1
         else
            i = j + i - 1
         end if

         if (present(before)) then
            if (j .gt. 1) then
               if (index(before, string(j - 1:j - 1)) .eq. 0) then
                  sub = sub // string(j:i - 1)
                  cycle
               end if
            end if
         end if

         if (present(insert)) sub = sub // insert

         if (present(ratio)) then
            if (ratio) then
               write (tmp, "(I0, ':', I0)") i - j, beats
               sub = sub // trim(tmp)
            end if
         end if

         if (present(keep)) then
            if (keep) sub = sub // string(j:i - 1)
         end if

         if (present(replace)) then
            sub = sub // repeat(replace, i - j)
         end if
      end do
   end function sub
end program guitar
