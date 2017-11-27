# Visual List Editor (VLE)
![vle](https://github.com/honix/Visual-List-Editor/blob/master/wiki/fibonacci.png)

## Status
VLE research is completed for today. It demonstrates visual way of lisp programming. This knowledge will be used for further projects.

## In short
VLE is a lisp expressions visual editor. It works in link with Common Lisp.

VLE allows to write programs using kind of [abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree).

## Story
*"Maybe if lisp started today, we would a syntax of lisp look like trees"* -- **Hal Abelson** ([1986 lecture](https://www.youtube.com/watch?v=XYKRVNQ_MqE&feature=youtu.be&t=34m33s))

## Modeling list
Try to enter
```+ Enter 10 Enter * Enter Enter 25 Enter 2```

This will make list
```(+ 10 (* 25 2))```

## Run from source
Before run VLE check that you dependencies ready

**Linux**

- Ubuntu (apt-get): ```sudo apt-get install libffi6 libffi-dev libsdl2 libsdl2-image libsdl2-ttf```

- Fedora (dnf): ```sudo dnf install libffi libffi-dev SDL2 SDL2_image SDL2_ttf```

- ArchLinux (pacman): ```sudo pacman -S libffi sdl2 sdl2_ttf sdl2_image```

**Windows**

- Install [MinGW](https://sourceforge.net/projects/mingw/files/Installer) with ```mingw32-base``` package, and copy ```ffi.h``` and ```ffitarget.h``` to ```...\MinGW\include```

- Add ```...\MinGW\bin``` to PATH environment variable (make able common lisp to call ```gcc```)

- Put dll's to your SBCL / CCL folder, right with *.exe (for x86 there is collected already, check ```deps/windows-deps.zip```)

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
