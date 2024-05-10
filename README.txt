MAENMPC -- Ma_Sys.ma Erlang Ncurses Music Player Client

-- Keybindings --

Aligned with ncmpc [...] := planned

UP [k]        select one item up
DOWN [j]      select one item down
PAGEUP        ...
PAGEDOWN      ...
LEFT [+]      decrease volume (-1)
RIGHT [-]     increase volume (+1)
s [BACKSPACE] stop
P             toggle pause (start playback after stop)
r             toggle repeat mode
z             toggle random mode
y             toggle single mode
C             toggle consume mode
x             toggle crossfade mode

Planned (partially aligned with ncmpc)

ENTER         play (enable/disable output on other screen)
a             append song to queue
HOME [gg]     select first item
END [G]       select last item
              NB: Conflicting key assignment `G` on purpose
/, ?, n, p    search inside screen
F10, q, Q     exit
DELETE, d     delete song from queue
CTRL-K/J      move item in queue down/up
>, <          next track, prev track                (optional, think about it)

Other planned

A             append song to queue right after the current song
*, #          rate up (*=+2) and down (#=-2), reset to default if beyond limit
R             start/reset radio          (relevant for radio page)
T             stop radio                 (relevant for radio page)
p             toggle podcast processing  (relevant for radio page)
SPACE         toggle expand/contract all (relevant for list page)
              NB: Conflicting key assignment for SPACE on purpose

-- Other Keybindings by ncmpc not implemented by maenmpc --

1-<n>      page selection
B          cursor movement
b          seek backward
c          clear queue
CTRL-A     cursor movement
CTRL-E     cursor movement
CTRL-L     Refresh screen
CTRL-U     trigger music db update
e          Add or edit lyrics
ESC        interrupt retrieval
f          seek forward
G          locate song in browser
"          Go to parent directory
!          Go to root directory
H          cursor movement
i          View the selected and currently playing song
.          Jump to
K          key configuration screen
L          cursor movement
m          Change search mode
M          cursor movement
N          cursor movement
>          next track
o          Crop
`          page selection
<          prev track
SHIFT-TAB  page selection
SPACE      select/deselect song in queue
S          Save queue/lyrics...
TAB        page selection
t          Select all listed items
u          Download lyrics for currently plaing song
w          Toggle find mode
Z          shuffle queue
