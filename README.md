# Lire
![lire](https://github.com/honix/Lire/blob/master/wiki/specials-test.png)

*"Maybe if lisp started today, we would a syntax of lisp look like trees"* -- **Hal Abelson** ([1986 lecture](https://www.youtube.com/watch?v=XYKRVNQ_MqE&feature=youtu.be&t=34m33s))

## In short
Lire is a Lisp expressions visual editor, it allows you to write programs using kind of [abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree). It runs on top of Common Lisp.

Lire is work in progress project. It is also my diploma thesis.

## Modeling list
Try to enter
```+ Enter 10 Enter * Enter Enter 25 Enter 2```

This will make list
```(+ 10 (* 25 2))```

## Try it out!
Check the [releases page](https://github.com/honix/Lire/releases). There is ready to go Windows executable, as it is little hard to build on this system.

## Run from source
Lire is well tested to work with [SBCL](http://www.sbcl.org). Before run Lire check that you dependencies ready:

**Linux (x86 and x64)**

- Ubuntu (apt-get): ```sudo apt-get install libffi6 libffi-dev libsdl2 libsdl2-image libsdl2-ttf```

- Fedora (dnf): ```sudo dnf install libffi libffi-devel SDL2 SDL2_image SDL2_ttf```

- ArchLinux (pacman): ```sudo pacman -S libffi sdl2 sdl2_ttf sdl2_image```

**Windows (x86)**

- Install [MinGW](https://sourceforge.net/projects/mingw/files/Installer) with ```mingw32-base``` package, and copy ```ffi.h``` and ```ffitarget.h``` from ```deps/windows``` to ```...\MinGW\include```

- Add ```...\MinGW\bin``` to the PATH environment variable (make able Common Lisp to call ```gcc```)

- Put the dll's to your SBCL folder, right with ```sbcl.exe``` (for x86 there is collected already, check ```deps/windows```)

**Lets go**

```cd your/path/to/Lire``` *and* ```sbcl --load lire.lisp```

## Controls
Action | Bind
-------|------
Place cursor (blue cross) or select node | Left-mouse-button click
Select area or drag node | Left-mouse-button drag
Accumulative select | Shift
Copy selected nodes | Ctrl-C
Paste copied nodes | Ctrl-V
Pan | Right-mouse-button drag
Zoom | Scroll
Create new node | (enter node name) Enter
Create list node (wraps arguments) | Space Enter
Create dot node (does nothing with argument) | . Enter
Switch to last node | Enter
Evaluate tree | Tab or Double-left click
Delete node | Delete
Connect nodes | Right-mouse-button drag from children to parent
Connect selected (last is a parent)| Ctrl-L
Lost connections | Ctrl-K
Move cursor up & down | Up-arrow & Down-arrow
Move cursor left & right | Left-arrow & Right-arrow
