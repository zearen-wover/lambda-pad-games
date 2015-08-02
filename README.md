# lambda-pad game configs

This is a collection of pre-built lambda-pad game configurations using Robot.

# Using

First, install [lambda-pad](http://github.com/zearen-wover/lambda-pad.git)
There are then two ways to use these files.  You may either:

- **Install the library**: Just use cabal to install this as a library.  This is
  quick and easy, but you're stuck with the defaults.
- **Copy into config directory**: `git clone` this repository and copy the
  configurations into the config directory.  This takes a little more work, but
  allows you to tweak the configs.

The two methods may be used together, of course.

## Install the Library

First install this.

    git clone http://github.com/zearen-wover/lambda-pad-games.git
    cabal configure
    cabal build
    cabal install

Then in your config (`$HOME/.config/lambda-pad/lambda-pad.hs`), write:

    import Game.LambdaPad
    import Game.LambdaPad.Games.Minecraft ( minecraft )

    main :: IO ()
    main = lambdaPad defaultLambdaPadConfig
        { gameConfigs = [ minecraft ]
        }

Add any other game(s) you want, then start `lambda-pad` !

    cd
    lambda-pad

## Copying into Config Directory

Download this to some location and `cd` into the `Games` directory.

    git clone http://github.com/zearen-wover/lambda-pad-games.git
    cd lambda-pad-games/src/Game/LambdaPad/Games

Next, copy your desired game into the config directory.  I prefer to organize
these under a `Games` sub-directory.

    mkdir -p $HOME/.config/lambda-pad/Games
    cp Minecraft.hs $HOME/.config/lambda-pad/Games

Then cd into the config directory and edit the copied config.

    cd $HOME/.config/lambda-pad
    $EDITOR Games/Minecraft.hs

First, we need to convert the module declaration to reflect it's new home and do
our tweaking.

    - module Game.LambdaPad.Games.Minecraft ( minecraft ) where
    + module Games.Minecraft ( minecraft ) where
    ...

Next we edit the config.

    $EDITOR lambda-pad.hs

And add the tweaked game

    import Game.LambdaPad
    import Games.Minecraft ( minecraft )

    main :: IO ()
    main = lambdaPad defaultLambdaPadConfig
        { gameConfigs = [ minecraft ]
        }

Repeat for any other game(s) you want, then start `lambda-pad` !

    cd
    lambda-pad
