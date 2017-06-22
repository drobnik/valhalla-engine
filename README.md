# valhalla-engine [![Build Status](https://travis-ci.com/drobnik/valhalla-engine.svg?token=WzzDh4VCxkMCN8q8FX4r&branch=master)](https://travis-ci.com/drobnik/valhalla-engine)
### Purely functional 2D game framework sprinkled with _some ugly IORefs_

This is an implementation of 2D tile-based game engine in Haskell done as
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
![Sample game screen][screen]

[screen]: https://lh3.googleusercontent.com/w0ncCzuZ-miOYxtv0OWwF6DCBavFhvt_0Rz_VMIo8dQKaYQ-6Cbzs27DqZqQu-CX_8aAP2xS2VPhwlayAAIWOo6QD9XSOF2zgek8Z2i_8Su1oAjNtACW1P1mZiE8K-ixzTz4NclzgIuQshLo-lnu1PaX9QIU5HDgJQWhP_8KuG2BHXQD6O5vlZrmZCryFO883RVD032Q6c9fMbHFZaeEEk4vlnDsu76dvaW1cHXLCFxlj9kZsi5tGEVvLWgGWtgjZN8BG0Ao6M04_vWxgmD6owiL5_5rS5JhESJL8AY6YhEVk0aIPE0215Xll6LkSguQhC5CuYQk5-UrY1yG4TL7QsA2c5tbeVgMOYcPAi3czwGBsg7LrEzdPn6k4CPQw5Ad3kxQTLl7HOw7KINdAahI-mU3_b0KeUOpXJfUOBe-HWyoiq8dNMy_B_dS6QAkeb8gJqS8se30cqhFEP7pPRwyegbf5d-_eI9naaJO0DRDHM8v5lVdi8ZaHhEsM5fwLzqZ_z4czqx_G9Qav1bbVTmeln981i-028v8TDkywa-05Z_3RErgQE4hmAAijcSpdCzGtNCUAoUAEh90PGe085J9IlQ1bC9cjLiDBgvt-32QKoRtXP8utEv-=w639-h500-no "valhalla sample game"

\* - the game logic and collisions are turned off for now, sorry about that.


## Documentation
To generate Haddock for this project, simply type: `stack haddock`.


## 'Valhalla in less than 5 minutes' tour
**[TODO]**
