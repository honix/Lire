# Visual List Editor (VLE)
![vle](https://github.com/honix/Visual-List-Editor/blob/master/wiki/kung-fu.png)

## Status
VLE in early-early development phase.

## In short
VLE is a lisp expressions visual editor. It works in link with Common Lisp (SBCL implementation).
It allows to write programs using visual representations of s-expressions.

## Story
*...in development...*

## Modeling list
Try to enter
```+ Enter 10 Enter * Enter Enter 25 Enter 2```

This will make list
```(+ 10 (* 25 2))```

## Run from source
Before run VLE check that you dependencies ready

**Linux**

- Ubuntu (and Debian family): ```sudo apt-get install libffi6 libffi-dev libsdl2 libsdl2-image libsdl2-ttf```

- Fedora: ```sudo dnf install libffi libffi-dev SDL2 SDL2_image SDL2_ttf```

**Windows**

- Install [MinGW](https://sourceforge.net/projects/mingw/files/Installer) with ```mingw32-base``` package, and copy ```ffi.h``` and ```ffitarget.h``` to ```...\MinGW\include```

- Add ```...\MinGW\bin``` to PATH environment variable (make able sbcl to call ```gcc```)

- Put dll's to your SBCL folder, right with sbcl.exe (for x86 there is collected already, check ```deps/windows-deps.zip```)

**Lets go**

```cd your/path/to/vle``` *and* ```sbcl --load main-with-utf8.lisp```

## Controls
Action | Bind
-------|------
Place cursor (blue cross) or select node | Left-mouse-button click
Select area or drag node | Left-mouse-button drag
Accumulative select | Shift
Pan | Right-mouse-button drag
Zoom | Scroll
Create new node | (enter node name) Enter
Switch to last node | Enter
Evaluate tree | Tab or Double-left click
Delete node | Delete
Connect nodes | Right-mouse-button drag from children to parent
Connect selected (last is a parent)| Key-Pad-7
Lost connections | Key-Pad-1
Move cursor up & down | Key-Pad-8 & Key-Pad-2
Move cursor left & right | Key-Pad-4 & Key-Pad-6
