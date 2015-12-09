## Tiny Rick

GL livecoding text editor in Haskell

# TODOs
[ ] Current indentation

[x] Fix buffer uniform clobbering
[ ] Finish vive clicking
[ ] Only save cursor position and active rick from old state
    so we can update text buffers and positionings
[ ] Scrolling (use fgPointSize * length (elemIndexL '\n') to find max height)
  [ ] Define maxHeight for text windows and clip outside of it

[x] Clickable cursor position (use cursorPos/fgPointSize to find line, then search for character using kerning)

[ ] Abstract into MVP + Filename + HasFocus IORef so we can render many of these

[ ] Text measurement


[x] currentColumn state that is remembered when pressing up and down, and only changed with left and right

[x] Key repeat support
[x] Copy/paste support

[x] Store cursor pos for halive usage
