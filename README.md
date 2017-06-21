# valhalla-engine [![Build Status](https://travis-ci.com/drobnik/valhalla-engine.svg?token=WzzDh4VCxkMCN8q8FX4r&branch=master)](https://travis-ci.com/drobnik/valhalla-engine)
### Purely functional 2D game framework

This is an implementation of 2D (tile-based) game engine in Haskell done as
a part of my Bachelor Thesis *"Implementing 2D Game Engine in Haskell"*. \
Game events and engine loop is implemented using the concept of monads and mutable [IORef](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-IORef.html) references.

The created library delivers ready-to-use components such as:

+ Rendering module with simpe 'RenderCommand' pipeline (greatly inspired
by [HGE2D](https://github.com/I3ck/HGE2D/blob/master/src/HGE2D/Datas.hs#L71-L84))

+ Simple, naïve maps and assets loading

+ Simple keyboard handling

+ Window and Content Manager

+ Collision detection

The project was developed using [stack](https://docs.haskellstack.org/en/stable/README/)
tool and GHC 7.10.3 compiler [TODO: Check against other versions]. \
Rendering pipeline and graphics components are programmed using
[SDL2](https://hackage.haskell.org/package/sdl2-2.2.0/docs/SDL.html) Haskell bindings.

___
**Note:** Many fatures are missing and the project should be treated as a proof-of-concept
instead of fully operating library.
___

## Requirements

### Linux

Before jumping straight for `stack install` (if you have
[one](https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux) already),
make sure you have `libsdl2-dev` on board:
```
$ sudo apt-get install libsdl2-dev
```

### Windows

There may occur some issues on Windows while configuring stack but if you want,
you can
[give it a try](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).
Additionally, you would need
[this](http://lazyfoo.net/tutorials/SDL/01_hello_SDL/windows/mingw/index.php).

[TODO: how about Win?]


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

## Documentation
To generate Haddock for this project, simply type: `stack haddock`.

## 'Valhalla in less then 5 minutes' tour
**[TODO]**
