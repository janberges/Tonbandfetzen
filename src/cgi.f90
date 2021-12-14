! For standalone use on server, compile with FFLAGS='-static -O3'.

program cgi
   use constants
   use interpreter
   use io
   use riff
   implicit none

   type(audio) :: music

   character(:), allocatable :: query
   character(*), parameter :: example &
      = "$22050 T pyth M A2'8 W ,5 A2' A3' E4' A4' C#v5' E5' Gz5' A5'"
   integer, parameter :: limit = 1000000

   query = decode(environment_variable('QUERY_STRING'))

   if (query .eq. ' ') then
      write (*, '(A, /, 1000(:, /, A))') "Content-type: text/html", &
         "<!DOCTYPE html>", &
         "<html>", &
         "  <head>", &
         "    <title>Tonbandfetzen</title>", &
         "    <style type='text/css'>", &
         "      body, #mel, #color {", &
         "        font-family: 'Liberation Sans', Helvetica, sans-serif;", &
         "        font-weight: bold;", &
         "      }", &
         "      #frame {", &
         "        position: relative;", &
         "      }", &
         "      #frame, #color, #mel {", &
         "        height: 200px;", &
         "      }", &
         "      #color, #mel {", &
         "        position: absolute;", &
         "        resize: none;", &
         "        overflow-y: scroll;", &
         "        padding: 1px;", &
         "        margin: 0;", &
         "        border: 1px solid #404040;", &
         "        white-space: pre-wrap;", &
         "        word-wrap: break-word;", &
         "      }", &
         "      #mel {", &
         "        color: #00000000;", &
         "        background: #00000000;", &
         "        caret-color: #000000ff;", &
         "      }", &
         "      #color, #mel, #play, #ctrl {", &
         "        display: block;", &
         "        box-sizing: border-box;", &
         "        width: 500px;", &
         "      }", &
         "      .N {", &
         "        color: #bf8040;", &
         "      }", &
         "      .L {", &
         "        color: #afdf00;", &
         "      }", &
         "    </style>", &
         "    <script type='text/javascript'>", &
         "      function enter() {", &
         "        document.getElementById('play').disabled = false", &
         "        var m = document.getElementById('mel').value", &
         "        m = m.replace(/&/g, '&AMP;')", &
         "        m = m.replace(/</g, '&LT;')", &
         "        m = m.replace(/[\d.:]+/g, '<SPAN CLASS=""N"">$&</SPAN>')", &
         "        m = m.replace(/[a-z#]+/g, '<SPAN CLASS=""L"">$&</SPAN>')", &
         "        m = m.replace(/\n$/g, '$&&nbsp;')", &
         "        document.getElementById('color').innerHTML = m", &
         "      }", &
         "      function move() {", &
         "        document.getElementById('color').scrollTop =", &
         "          document.getElementById('mel').scrollTop", &
         "      }", &
         "      function play() {", &
         "        document.getElementById('play').disabled = true", &
         "        document.getElementById('wav').src = '?'", &
         "          + encodeURIComponent(", &
         "            document.getElementById('mel').value)", &
         "        document.getElementById('ctrl').load()", &
         "        document.getElementById('ctrl').play()", &
         "      }", &
         "    </script>", &
         "  </head>", &
         "  <body onload='enter()'>", &
         "    <h1>Tonbandfetzen</h1>", &
         "    <h2>Input</h2>", &
         "    <div id='frame'>", &
         "      <div id='color'></div>", &
         "      <textarea id='mel' spellcheck='false'", &
         "        oninput='enter()' onscroll='move()'>", &
         example, &
         "</textarea>", &
         "    </div>", &
         "    <button id='play' onclick='play()'>Play</button>", &
         "    <h2>Output</h2>", &
         "    <audio id='ctrl' controls>", &
         "      <source id='wav' type='audio/wave'>", &
         "    </audio>", &
         "  </body>", &
         "</html>"
   else
      call play(query, music, limit)
      if (music%points .eq. 0) call play("$22050 |1:6 E3' C3'", music)

      music%amplitude = 1.0_dp

      write (*, "('Content-type: audio/wav', /)")
      call write_riff('stdout', music)
   end if

contains

   function decode(code) result(url)
      character(:), allocatable :: url

      character(*), intent(in) :: code

      integer :: i, n

      url = code

      i = 1
      do
         n = index(url(i:), '%')
         if (n .eq. 0) exit
         i = n + i

         read (url(i:i + 1), '(Z2)') n
         url = url(:i - 2) // char(n) // url(i + 2:)
      end do
   end function decode
end program cgi
