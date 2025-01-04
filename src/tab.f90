module tab
   implicit none
   private

   public :: preprocess, strip, matches, sub

   integer :: beats

contains

   function preprocess(tablature) result(notes)
      character(:), allocatable :: notes

      character(*), intent(in) :: tablature

      integer :: lower, upper, from, till
      logical :: first

      character :: c
      character, parameter :: lf = achar(10), cr = achar(13)
      character(2), parameter :: rn = cr // lf
      character(:), allocatable :: nl, line, bar, bars

      notes = ''

      first = .true.

      lower = 1
      do while (lower .le. len(tablature))
         upper = scan(tablature(lower:), rn)

         if (upper .eq. 0) then
            upper = len(tablature)

            nl = ''
         else
            upper = lower + upper - 2

            nl = tablature(upper + 1:upper + 1)

            if (upper + 2 .le. len(tablature)) then
               c = tablature(upper + 2:upper + 2)

               if (scan(c, rn) .eq. 1 .and. c .ne. nl) nl = nl // c
            end if
         end if

         line = trim(tablature(lower:upper))

         lower = upper + len(nl) + 1

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

            notes = notes // bars // nl
         else
            if (len(strip(line)) .eq. 0) first = .true.

            notes = notes // line // nl
         end if
      end do
   end function preprocess

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
      character(1024) :: tmp

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
end module tab
