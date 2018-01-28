# Lire
![lire](https://github.com/honix/Lire/blob/master/wiki/pythagoras.png)

*"Maybe if lisp started today, we would a syntax of lisp look like trees"* -- **Hal Abelson** ([1986 lecture](https://www.youtube.com/watch?v=XYKRVNQ_MqE&feature=youtu.be&t=34m33s))

## Status
At this time i actively refactoring the system. Points is: moving to glup window system, introducing classes for better reusability, pixel as position unit. Update will pushed in some weeks.

## In short
Lire is a lisp expressions visual editor, this allows you to write programs using kind of [abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree). It runs on top of Common Lisp.

Lire is work in progress project.

## Modeling list
Try to enter
```+ Enter 10 Enter * Enter Enter 25 Enter 2```

This will make list
```(+ 10 (* 25 2))```

## Run from source
Before run Lire check that you dependencies ready

**Linux**

- Ubuntu (apt-get): ```sudo apt-get install libffi6 libffi-dev libsdl2 libsdl2-image libsdl2-ttf```

- Fedora (dnf): ```sudo dnf install libffi libffi-dev SDL2 SDL2_image SDL2_ttf```

- ArchLinux (pacman): ```sudo pacman -S libffi sdl2 sdl2_ttf sdl2_image```

**Windows**

- Install [MinGW](https://sourceforge.net/projects/mingw/files/Installer) with ```mingw32-base``` package, and copy ```ffi.h``` and ```ffitarget.h``` to ```...\MinGW\include```

- Add ```...\MinGW\bin``` to PATH environment variable (make able common lisp to call ```gcc```)

- Put dll's to your SBCL / CCL folder, right with *.exe (for x86 there is collected already, check ```deps/windows-deps.zip```)

**Lets go**

```cd your/path/to/Lire``` *and* ```sbcl --load lire.lisp```

## Controls
Action | Bind
-------|------
Place cursor (blue cross) or select node | Left-mouse-button click
Select area or drag node | Left-mouse-button drag
Accumulative select | Shift
Pan | Right-mouse-button drag
Zoom | Scroll
Create new node | (enter node name) Enter
Create list node (wraps arguments) | Space Enter
Create dot node (does nothing with argument) | . Enter
Switch to last node | Enter
Evaluate tree | Tab or Double-left click
Delete node | Delete
Connect nodes | Right-mouse-button drag from children to parent
Connect selected (last is a parent)| Key-Pad-7
Lost connections | Key-Pad-1
Move cursor up & down | Key-Pad-8 & Key-Pad-2
Move cursor left & right | Key-Pad-4 & Key-Pad-6
