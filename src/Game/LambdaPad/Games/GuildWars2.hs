{-# LANGUAGE TemplateHaskell #-}
module Game.LambdaPad.Games.GuildWars2 ( guildWars2 ) where

import Control.Monad ( when )
import Control.Lens ( (.=) )
import Control.Lens.TH ( makeLenses )
import Data.Algebra.Boolean (Boolean(..))
import Prelude hiding ( (&&), (||),  not )

import Game.LambdaPad.GameConfig
import Game.LambdaPad.GameConfig.Robot

import qualified Test.Robot.Types as K

data GuildWars2 = GuildWars2
    { gw2Conn :: !Connection
    , gw2MouseSpeed :: !Float
    , _gw2MouseResidual :: !(Float, Float)
    , _gw2ScrollResidual :: !(Float, Float)
    , _gw2UndoDir :: LambdaPad GuildWars2 ()
    , _gw2LeftStickPress :: !StickPressed
    , _gw2Panning :: !Bool
    }
makeLenses ''GuildWars2

instance HasRobot GuildWars2 where
    getConn = gw2Conn

guildWars2 :: Float -> PackagedGameConfig
guildWars2 mouseSpeed = package $ GameConfig
    { gameName = "gw2"
    , newUserData = do
          conn <- connect
          (width, height) <- getScreenSize conn
          return $ GuildWars2
              { gw2SConn = conn
              , gw2MouseSpeed = mouseSpeed * fromIntegral (max width height)
              , _gw2MouseResidual = (0, 0)
              , _gw2ScrollResidual = 0
              , _gw2UndoDir = return ()
              , _gw2LeftStickPressed = emptyStickPressed
              , _gw2Panning = False
              }
    , onStop = const $ return ()
    , onEvents = do
          buttonAsKeys a true [K._Space]
          buttonAsKeys x true [K._1]
          buttonAsKeys y true [K._2]
          buttonAsKeys b true [K._3]
          buttonAsKeys lb true [K._4]
          buttonAsKeys rb true [K._5]

          buttonAsKeys lb shiftMode [K._6]
          buttonAsKeys x shiftMode [K._7]
          buttonAsKeys y shiftMode [K._8]
          buttonAsKeys b shiftMode [K._9]
          buttonAsKeys rb shiftMode [K._0]

          withDPad gw2UndoDir
              [ dirAsKey N true [K._F1]
              , dirAsKey E true [K._F2]
              , dirAsKey S true [K._F3]
              , dirAsKey W true [K._F4]

              , dirAsKey N shiftMode [K._T]
              , dirAsKey E shiftMode [K._Tab]
              , dirAsKey S shiftMode [K._Ctrl, K._T]
              , dirAsKey W shiftMode [K._Shift, K._Tab]
              ]

          buttonAsKeys rs true [K._F]
          buttonAsKeys rs mouseMode [K.rightButton]
          buttonAsKeys rb mouseMode [K.leftButton]

          buttonAsKeys home true [K._Escape]
          buttonAsKeys start true [K._I]
          buttonAsKeys back true [K._Grave]
          buttonAsKeys start shiftMode [K._H]
          buttonAsKeys back shiftMode [K._M]
          
          buttonAsKeys ls true [K._V]
          stickAsKeys (gw2LeftStickPress.ePress) leftStick (Horiz (>0.2))
              true [K._E]
          stickAsKeys (gw2LeftStickPress.wPress) leftStick (Horiz (<(-0.2)))
              true [K._Q]
          stickAsKeys (gw2LeftStickPress.nPress) leftStick (Vert (>0.2))
              true [K._W]
          stickAsKeys (gw2LeftStickPress.sPress) leftStick (Vert (<(-0.2)))
              true [K._S]

          stickAsKeys gw2Panning rightStick (Push (>0.05))
              (not mouseMode) [K.rightButton]
          -- If we enter mouse mode, we should stop panning.
          onTrigger leftTrigger (mouseMode && whenUser (^.gw2Panning)) $ do
              releaseKeys [K.rightButton]
              gw2Panning .= False

          onTick $ do
            isPanning <- use $ gw2Panning
            if isPanning
              then stickAsMouse 0.05 400 gw2MouseResidual rightStick
              else do 
                isMouse <- isPad mouseMode
                isScroll <- isPad shiftMode
                when (isMouse) $ if isScroll
                  then stickAsScroll 0.05 10 gw2MouseResidual rightStick
                  else do
                    mouseSpeed' <- fmap gw2MouseSpeed get
                    stickAsMouse 0.05 mouseSpeed' gw2MouseResidual rightStick
    }

shiftMode :: Filter user
shiftMode = with rightTrigger $ Pull (>0.2)

mouseMode :: Filter user
mouseMode = with leftTrigger $ Pull (>0.2)
