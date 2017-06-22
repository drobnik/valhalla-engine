# valhalla-engine [![Build Status](https://travis-ci.com/drobnik/valhalla-engine.svg?token=WzzDh4VCxkMCN8q8FX4r&branch=master)](https://travis-ci.com/drobnik/valhalla-engine)
### Purely functional 2D game framework sprinkled with _some ugly IORefs_

This is the implementation of 2D tile-based game engine in Haskell done as
a part of my Bachelor Thesis *"Implementing 2D Game Engine in Haskell"*.
Game events and engine loop are implemented using the concept of monads and mutable [IORef](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-IORef.html "IORef docs")
references.


___
**Note:** Many fatures are missing and project should be treated as a proof-of-concept
instead of fully operating library.
___



The created library delivers ready-to-use components such as:

+ Rendering module with simpe `RenderCommand` pipeline (greatly inspired
by [HGE2D](https://github.com/I3ck/HGE2D/blob/master/src/HGE2D/Datas.hs#L71-L84
"HGE2D RenderInstruction definition"))

+ Simple, naïve maps and assets loading

+ Simple keyboard handling

+ Window and Content Manager

+ Collision detection


The project was developed using [stack](https://docs.haskellstack.org/en/stable/README/
"stack") tool and GHC 7.10.3 compiler [TODO: Check against other versions]. \
Rendering pipeline and graphics components are programmed using
[SDL2](https://hackage.haskell.org/package/sdl2-2.2.0/docs/SDL.html "SDL2 docs")
Haskell bindings.



## Requirements


### Linux

Before jumping straight for `stack install` (if you have
[one](https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux
"How to install stack on Linux") already), make sure you have `libsdl2` on board:
```
$ sudo apt-get install libsdl2
```
Or you may want to [compile](https://wiki.libsdl.org/Installation#Linux.2FUnix
"Compiling SDL2") it yourself.


### Windows

There are some issues with stack configuration on Windows but if you want,
you can
[give it a try](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).\
Additionally, you may need
[this](http://lazyfoo.net/tutorials/SDL/01_hello_SDL/windows/mingw/index.php).



## Installation

1. Get the sources
```
git clone https://github.com/drobnik/valhalla-engine
cd valhalla-engine
```

2. Install the depedencies
```
stack install --dependencies-only
```

3. Build the project
```
stack build
```

4. Enjoy



## Running
If you want to see sample ~~game*~~ graphics and movement, type:
```
stack exec valhalla-engine-exe
```
\* - the game logic and collisions are turned off for now, sorry about that.

![Screen](http://i.imgur.com/K00B1qT.png)


## Documentation
To generate Haddock for this project, simply type: `stack haddock`.


## 'Valhalla in less than 5 minutes' tour
**[TODO]**
