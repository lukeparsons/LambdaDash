module Main where

import Graphics.Gloss
    ( polygon, rectangleSolid, pictures, text, scale, translate, color, loadBMP, greyN, blue, light, green, dark, black, Display(InWindow), Picture, Color )
import System.Random ( newStdGen, Random(randomR), StdGen, mkStdGen, RandomGen, setStdGen )
import GHC.Float (powerFloat, int2Float, float2Int)
import Graphics.Gloss.Interface.IO.Game (playIO, Event (EventKey), Key (SpecialKey, Char), KeyState (Down))
import Graphics.Gloss.Interface.Environment (getScreenSize)

--define some constants

playerStartingPositionX :: Float
playerStartingPositionX = -50

geometryStartingHeight :: Float
geometryStartingHeight = -15

rightMostPoint :: Float -- right most point on the screen (where new geometry spawns)
rightMostPoint = 500

gapOffset :: Float -- standard gap between presets
gapOffset = 30

deathHeight :: Float
deathHeight = -200

playerSizeXY :: (Float, Float)
playerSizeXY = (10, 10)

-- The DashWorld data type stores core information about the player and game state
data DashWorld = World {
    playerHeight :: Float,
    playerSize :: (Float, Float),
    playerShouldJump :: Bool,
    playerInitiateJumpTimeHeight :: (Float, Float),
    geometryList :: [GeometryData],
    geometryVelocity :: Float,
    gameElapsedTime :: Float,
    playerDead :: Bool,
    finalScore :: Int,
    renderMenu :: Bool
}

initialWorld :: DashWorld
initialWorld = World {
    playerHeight = 0,
    playerSize = playerSizeXY,
    playerShouldJump = False,
    playerInitiateJumpTimeHeight = (0, 0),
    geometryList = groundList,
    geometryVelocity = -80,
    gameElapsedTime = 0,
    playerDead = False,
    finalScore = 0,
    renderMenu = True
}

{- The player can collide with geometry in three ways
 - Not at all, in which case "None"
 - On Top of the geometry, in which case "Top" and they will travel along until the end of geometry
 - The Side of the geometry which kills the player (also used for spikes and fireballs)
 -}
data PlayerCollision = None | Top | Side
    deriving Eq

{- Ground is the standard geometry that the player can traverse on
 - Spike and Fireball are two obstacles that the player must avoid
 - Spikes are stationary whereas fireballs move
-}
data GeometryType = Ground | Spike | Fireball
    deriving Eq

data GeometryData = Geometry {
    geomLoc :: (Float, Float),
    geomSize :: (Float, Float),
    geomType :: GeometryType
}
    deriving Eq

{- The location field of geometry is at it's centre
 - We can get the horizontal bounds of the geometry by adding/subtracting half it's horizontal size from the centre for the right/left bound
-}
geometryHorizontalBounds :: GeometryData -> (Float, Float)
geometryHorizontalBounds geom = (fst (geomLoc geom) - 0.5 * fst (geomSize geom), fst (geomLoc geom) + 0.5 * fst (geomSize geom))

{- These standard ground types exist to give the player an easy introduction into a new play scene
- They also have specific locations so that the player doesn't spawn above no platform or cannot jump to the preset platforms
-}
ground1 :: GeometryData
ground1 = Geometry { geomLoc = (-40, -15), geomSize = (270, 20), geomType = Ground }

ground2 :: GeometryData
ground2 = Geometry { geomLoc = (260, -15), geomSize = (270, 20), geomType = Ground }

ground3 :: GeometryData
ground3 = Geometry { geomLoc = (520, -15), geomSize = (180, 20), geomType = Ground }

groundList :: [GeometryData]
groundList = [ground1, ground2, ground3, fireball]

-- Create geometry based on the given arguments and set it's location to the right most position with a small offset to create a gap between platforms
makePreset :: Float -> (Float, Float) -> GeometryType -> GeometryData
makePreset height size gType = Geometry { geomLoc = (geomLocX, geomLocY), geomSize = size, geomType = gType }
                    where
                        geomLocX = rightMostPoint + gapOffset + 0.5 * fst size
                        geomLocY = height - 0.5 * snd playerSizeXY - 0.5 * snd size

{- Provide gaps between platforms in a preset 
 - Head and tail are partial functions and unsafe so this function will return an error if a preset with a length less than 2 is given
 - This is a scenario which does not make sense for this function
 - Otherwise, increase it's location by a factor of it's index in the list to provide a gap
 -}
processPreset :: Float -> [GeometryData] -> [GeometryData]
processPreset gapX preset = if length preset < 2 
                                then
                                    error "Must provide at least two elements to processPreset"
                                else
                                    head preset : zipWith (\x y -> x { geomLoc = (geomX x y, snd (geomLoc x)) }) (tail preset) [1 .. int2Float (length preset)]
                                        where
                                            geomX :: GeometryData -> Float -> Float
                                            geomX x y = fst (geomLoc x) + fst (geomSize x) * y + y * gapX

-- The full list of all the presets made below
presetList :: [[GeometryData]]
presetList = [varyingHeights, stairCase, spikePlatform, doubleSpikePlatform, squaresAlongPlatform, spikeStairCase, fireballPlatform, spikeAndFireball, 
                sinePlatform, cosPlatform]

obstacleSize :: (Float, Float)
obstacleSize = (9, 6)

spike :: GeometryData
spike = makePreset 8 obstacleSize Spike

fireball :: GeometryData
fireball = makePreset 10 obstacleSize Fireball

-- A spike and a fireball do not exist as seperate preset but as a part of exisiting geomotry so their locations must be at an offset to the geometry's location 
makeObstacle :: GeometryData -> Float -> Float -> GeometryData
makeObstacle obstacle xOffset yOffset = obstacle { geomLoc = (fst (geomLoc obstacle) + xOffset, snd (geomLoc obstacle) + yOffset) }

-- Can use mathematical sequences to define different presets 
varyingHeights :: [GeometryData]
varyingHeights = processPreset 30 (take 3 [makePreset (-x) (150, 20) Ground | x <- [30, 20, 10]] ++ fireballPlatform)

stairCase :: [GeometryData]
stairCase = processPreset 20 (take 4 [makePreset x (140, 20) Ground | x <- [5, 15 ..]])

-- Can place obstacles in specific locations along geometry
spikePlatform :: [GeometryData]
spikePlatform = [makePreset 0 (250, 20) Ground, makeObstacle spike 80 0]

doubleSpikePlatform :: [GeometryData]
doubleSpikePlatform = [makePreset 0 (400, 20) Ground, makeObstacle spike 100 0, makeObstacle spike 260 0]

squaresAlongPlatform :: [GeometryData]
squaresAlongPlatform = makePreset 0 (380, 20) Ground : smallSquares
                    where
                        smallSquares = take 2 [smallSquare { geomLoc = (fst (geomLoc smallSquare) + x, snd (geomLoc smallSquare))} | x <- [100, 250]]
                        smallSquare = makePreset 20 (10, 20) Ground

-- Can combine presets to create variations - this one combines the staircase with spikes
spikeStairCase :: [GeometryData]
spikeStairCase = map addSpike stairCase ++ stairCase
                    where
                        addSpike :: GeometryData -> GeometryData
                        addSpike platform = makeObstacle spike spikeX spikeY
                            where
                                spikeX = fst (geomLoc platform) - int2Float (length stairCase) * fst (geomSize platform) + 30
                                spikeY = snd (geomLoc platform) + snd (geomSize platform) - snd obstacleSize

-- Fireballs are fun as they provide another challenge for the player as they move quickly
fireballPlatform :: [GeometryData]
fireballPlatform = [makePreset 0 (150, 20) Ground, fireball]

spikeAndFireball:: [GeometryData]
spikeAndFireball = processPreset 20 [makePreset 0 (340, 20) Ground, makeObstacle spike 200 0] ++ fireballPlatform

-- Even more mathematical functions used to create presets - many possibilities
makeTrigPlatform :: (Float -> Float) -> [GeometryData]
makeTrigPlatform trigEquation = processPreset 40 (take 5 [makePreset (15 * trigEquation x) (140, 20) Ground | x <- [5, 15 ..]])

sinePlatform :: [GeometryData]
sinePlatform =  makeTrigPlatform sin

cosPlatform :: [GeometryData]
cosPlatform =  makeTrigPlatform cos

-- Uses a random generator to take a random element from a list
takeRandomFromList :: RandomGen g => g -> [a] -> a
{- !! is unsafe because it is a partial function. 
 - However in this case it is fine to use as we calculate a random index between the first index of the list and the last index so won't get out of range
 - The only issue would arise if an empty list was provided - in which case throw an error
-}
takeRandomFromList g xs = case length xs of
                            0 -> error "Must provide more than 0 elements to takeRandomFromList"
                            _ -> xs !! i
                                where
                                    i = fst (randomR (0, length xs - 1) g)

-- Selects a random colour from the list for the background on each launch of the game
background :: RandomGen g => g -> Color
background g = takeRandomFromList g colourList
    where
        colourList = [black, dark $ dark green, light $ light $ light blue, greyN 0.5]

{- Entry point for the game
- Loads sprites, creates the window and runs play function 
-}
main :: IO ()
main = do
        screenSize <- getScreenSize
        g <- newStdGen
        logo      <- loadBMP "assets/lambda.bmp"
        fireball1 <- loadBMP "assets/fireball_1.bmp"
        fireball2 <- loadBMP "assets/fireball_2.bmp"
        fireball3 <- loadBMP "assets/fireball_3.bmp"
        fireball4 <- loadBMP "assets/fireball_4.bmp"
        fireball5 <- loadBMP "assets/fireball_5.bmp"
        -- We need multiple fireball sprites to index through to create a cool animation
        let fireballs = [fireball1, fireball2, fireball3, fireball4, fireball5]
        playIO (window screenSize) (background g) 60 initialWorld (worldToPicture logo fireballs) inputHandler nextIteration


-- Create the window, place it at the center of the screen and set size to half the screen, based on the user's resolution
window :: (Int, Int) -> Display
window screenSize = InWindow "Dash" halfScreen center
                        where
                            halfScreen = (div (fst screenSize) 2, div (snd screenSize) 2)
                            center = (div (fst halfScreen) 2, div (snd halfScreen) 2)

{- Converts the world state to a renderable picture based on the locations and sizes of the player and geometry
 - Also take in sprites used for the player, menu background and the fireball animations
 - Returns a singular picture to for Gloss to draw
-}
worldToPicture :: Picture -> [Picture] -> DashWorld -> IO Picture
worldToPicture logo fireballs world =
                            let
                                menuText = color (dark blue) $ translate (-200) 100 $ scale 0.5 0.5 $ text "Lambda Dash"
                                controlsText = color (dark blue) $ translate (-275) 25 $ scale 0.3 0.3 $ text "Controls: Press Spacebar to jump"
                                easyModeText = color (dark blue) $ translate (-225) (-50) $ scale 0.3 0.3 $ text "Press E for Easy Mode"
                                hardModeText = color (dark blue) $ translate (-225) (-125) $ scale 0.3 0.3 $ text "Press H for Hard Mode"
                                menuScoreText = color (dark blue) $ translate (-100) (-200) $ scale 0.2 0.2 $ text ("Final Score: " ++ show (finalScore world))

                                menuLogo = scale 0.2 0.2 logo

                                scoreText = color (dark blue) $ translate (-350) 200 $ scale 0.2 0.2 $ text ("Score: " ++ show (float2Int (gameElapsedTime world)))

                                -- The player always stays in the same starting position but can move vertically
                                player = translate playerStartingPositionX (playerHeight world + 5) $ scale 0.01 0.01 logo

                                ground = pictures (map makeGround (geometryList world))

                                -- Generalise ground generation based on it's type
                                makeGround :: GeometryData -> Picture
                                makeGround geometry = uncurry translate (geomLoc geometry) $ uncurry color geometryInfo -- move to the correct position in the scene
                                    where
                                        xSize = fst (geomSize geometry)
                                        ySize = snd (geomSize geometry)

                                        -- Initalise the geometry's picture with colour (based on their type) and size
                                        geometryInfo :: (Color, Picture)
                                        geometryInfo
                                                    | geomType geometry == Ground = (light blue, uncurry rectangleSolid (geomSize geometry))
                                                    | geomType geometry == Spike = (greyN 0.2, polygon [ (-1 * xSize, -1 * ySize), (0, 0), (xSize, -1 * ySize), (0, 2 * ySize)])
                                                    | otherwise = (black, scale 0.15 0.15 selectFireballSprite)
                                                        where
                                                            -- Sequentially go through each fireball sprite by using a sin wave to get the index from the sprite list
                                                            -- !! is unsafe however in this case ok because it is bounded by the size of the fireball sprites list
                                                            selectFireballSprite = fireballs !! abs (round (sin (8 * gameElapsedTime world)) * (length fireballs - 1))
                                in
                                    if renderMenu world
                                        then
                                            pure (pictures [menuLogo, menuText, controlsText, easyModeText, hardModeText, menuScoreText]) -- Draw the menu
                                        else
                                            pure (pictures [player, ground, scoreText]) -- Draw the play scene

{- Takes in key press event to handle user input for the game to update the next frame with the user's request
 - Returns the World record with updated fields
-}
inputHandler :: Event -> DashWorld -> IO DashWorld
inputHandler (EventKey (SpecialKey space) Down _ _) world =
                                                        do
                                                            -- The player can only jump if on top of geometry
                                                            if playerColliding world == Top
                                                                then
                                                                    -- If a valid jump, set the initial time and height that they jumped from for calculations later
                                                                    pure world { playerShouldJump = True, playerInitiateJumpTimeHeight = (gameElapsedTime world, playerHeight world) }
                                                                else
                                                                    pure world
-- There are two difficulties in the game - easy and hard. The difference is the velocity of the geometry
inputHandler (EventKey (Char 'e') _ _ _) world = leaveMenu world initialWorld { geometryVelocity = -80 }
inputHandler (EventKey (Char 'h') _ _ _) world = leaveMenu world initialWorld { geometryVelocity = -100 }
inputHandler _ world = pure world

-- General function to exit the menu and start the game, if in the menu
leaveMenu :: DashWorld -> DashWorld -> IO DashWorld
leaveMenu world leaveMenuWorld
                        | renderMenu world = pure leaveMenuWorld { renderMenu = False }
                        | otherwise = pure world

{- Step to the next frame with updated world record. 
 - seconds is the delta time (time between frames) and the world is the World record from the current frame
 - Checks for scene change between the game and menu by checking the playerDead field
-}
nextIteration :: Float -> DashWorld -> IO DashWorld
nextIteration seconds world
                            | playerDead world = pure initialWorld { playerDead = False, finalScore = finalScore world }
                            | otherwise = updateWorldMovement seconds world <$> newStdGen -- Generate a new StdGen for random preset generation                    

{- Updates the movement of the player and the geometry
- Handles new geometry being added from the preset list and geometry which has been passed by the player being removed
- Returns an updated World record detailing changes to the player and geometry
-}
updateWorldMovement :: Float -> DashWorld -> StdGen -> DashWorld
updateWorldMovement seconds world randGen = world { geometryList = newGeometry, playerHeight = fst playerJump,
                                    playerShouldJump = snd playerJump, gameElapsedTime = updateElapsedTime, playerDead = checkPlayerDead, finalScore = getFinalScore }
                                        where
                                            newGeometry = updateGeometry seconds world randGen

                                            -- Get the total time the game has been played for by updating the record field by the delta time each frame 
                                            updateElapsedTime = gameElapsedTime world + seconds

                                            {- If the player falls out the world by reaching a very low height, return to menu. 
                                            If the player hits an obstacles they will also be set to this height to return to the menu -}
                                            checkPlayerDead :: Bool
                                            checkPlayerDead = playerHeight world <= deathHeight

                                            {- Update the final score each frame while playing
                                            But in a menu keep it the same so that the player can see their score from their previous attempt -}
                                            getFinalScore :: Int
                                            getFinalScore = if checkPlayerDead || renderMenu world then finalScore world else round updateElapsedTime

                                            -- Handle the different movement states for the player. Returns the height and whether the player is supposed to be jumping.
                                            playerJump :: (Float, Bool)
                                            playerJump
                                                        | playerColliding world == Side = (deathHeight, False)
                                                        | playerShouldJump world =
                                                            -- Only jump when the player aren't touching anything and we aren't at the start of the jump to ensure the player isn't stuck to the ground
                                                            if playerColliding world /= None && fst (playerInitiateJumpTimeHeight world) /= gameElapsedTime world
                                                                then
                                                                    (playerHeight world { playerHeight = playerHeight world + 1.5 }, False)
                                                                else
                                                                    (playerHeightCalculation seconds world { gameElapsedTime = updateElapsedTime }, True)
                                                        | playerColliding world == None = (playerHeightCalculation seconds world, False) -- Fall if not jumping and not colliding
                                                        | otherwise = (playerHeight world, False)

{- Handle geometry movement and rendering (adding new presets and removing completed platforms)
-  In the game, the geometry moves horizontally while the player moves vertically
-  This gives the illusion of the player moving left to right but means that we don't have to deal with moving a camera
-  Returns the updated geometry list with new positions
-}
updateGeometry :: Float -> DashWorld -> StdGen -> [GeometryData]
updateGeometry seconds world randGen = map (\x -> moveGeometry x seconds (geometryVelocity world)) newGeomList
    where
        newGeomList = newGeometryFromPreset (isOutsideRenderSpace (geometryList world)) randGen

-- Move the geometry from right to left to create the illusion of the player moving left to right
moveGeometry :: GeometryData -> Float -> Float -> GeometryData
moveGeometry geometry seconds geometryVel = geometry { geomLoc = (newX, currentY) }
    where
        (currentX, currentY) = geomLoc geometry
        newX = currentX + geomVel * seconds -- Constant, smooth speed of geometry by multiplying by delta time (seconds)
        geomVel
                | geomType geometry == Fireball = -170 -- Fireballs move faster otherwise they would be like a normal platform
                | otherwise = geometryVel

{- If all geometry's right point plus the offset between presets is less than the right most point constant, then spawn a new random preset in
 - Adding at this condition is crucial to ensure appropriate gaps between presets and to prevent more than once preset being spawned at once at the same position  
-}
newGeometryFromPreset :: [GeometryData] -> StdGen -> [GeometryData]
newGeometryFromPreset geometryList randGen = if all (\x -> snd (geometryHorizontalBounds x) + gapOffset < rightMostPoint) geometryList
                                        then
                                            takeRandomFromList randGen presetList ++ geometryList
                                        else
                                            geometryList

{- Filter out the geometry that have long since passed the player's position to keep the geometry list small 
 - Objects outside the viewport will be in clip space anyway by OpenGL through Gloss
 - But could otherwise end up with very long geometry list as player can hypothetically play forever
-}
isOutsideRenderSpace :: [GeometryData] -> [GeometryData]
isOutsideRenderSpace geometryList = filter (\x -> snd (geometryHorizontalBounds x) > (playerStartingPositionX * 12)) geometryList

-- Calculates and returns the players height for falling and jumping based on delta time, their current height and the time they started jumping from
playerHeightCalculation :: Float -> DashWorld -> Float
playerHeightCalculation seconds world = newHeight
    where
        currentHeight = playerHeight world

        newHeight = if playerShouldJump world
                        then
                            {- Equation s = ut + 0.5t^2 (displacement = initial velocity * time + 0.5 * time squared)
                             - This equation handles both the upwards jump and downwards fall in a quadratic form
                             - Able to do this because we calculate time as the difference of the the time elapsed to this frame and the time elapsed to when the player started the jump
                             - The player jumps and the geometry moves along at the same time, giving the illusion that the player has jumped forward
                            -}
                            initiateJumpHeight + initialJumpVelocity * (elapsedTime - initiateJumpTime) + 0.5 * gravityConstant * powerFloat (elapsedTime - initiateJumpTime) 2
                        else
                            -- Otherwise fall at a constant displacement
                            currentHeight - 0.8
        elapsedTime = gameElapsedTime world
        initialJumpVelocity = 70
        gravityConstant = -90
        initiateJumpTime = fst (playerInitiateJumpTimeHeight world)
        initiateJumpHeight = snd (playerInitiateJumpTimeHeight world)

-- Checks for the player colliding with geomotry and returns the type of collision
playerColliding :: DashWorld -> PlayerCollision
playerColliding world
                        | any (==Side) collisionValues = Side -- Side collisions take precedence - the game ends if this happens
                        | any (==Top) collisionValues  = Top 
                        | otherwise                    = None -- Finally if the player has no side or top collisions they can't be colliding with anything at all
                    where
                        collisionValues = map isColliding (geometryList world)

                        {- Check if the player is colliding with geometry
                         - Need to deal with Ground and Obstacle geometry type differently
                         - Ground is an exact calculation as precise collisions are important for the player to be able to jump and fall
                         - Obstacle collision is a more general calculation - precise collisions aren't important just need to make sure that we collide
                         - if the player is obviously colliding with an obstacle graphically 
                        -}
                        isColliding :: GeometryData -> PlayerCollision
                        isColliding geometry
                                            | geomType geometry == Ground = groundCollisionCheck (playerHeight world) (playerSize world) geometry
                                            | otherwise = obstacleCollisionCheck (playerHeight world) (playerSize world) geometry

{- Checks for collision by checking around the location of the obstacle as a general calculation
    - Spikes are triangles and fireballs are oval sprites - difficult to concisely calculate precise hitboxes for both
    - If collision with a obstacle is found return Side collision as this kills the player
-}
obstacleCollisionCheck :: Float -> (Float, Float) -> GeometryData -> PlayerCollision
obstacleCollisionCheck playerHeight playerSize geometry
                            | topCollision && horizontalLeftCollision && horizontalRightCollision = Side
                            | otherwise = None
                        where
                            topCollision             = playerHeight - 0.5 * snd playerSize - 0.5 <= snd (geomLoc geometry)
                            horizontalLeftCollision  = playerStartingPositionX - 0.5 * fst playerSize <= fst (geomLoc geometry) + snd (geomSize geometry)
                            horizontalRightCollision = playerStartingPositionX + 0.5 * fst playerSize >= fst (geomLoc geometry) - snd (geomSize geometry)

{- Precise calculation for both Side and Top collisons with the ground
 - Simple calculation as Ground is always a rectangle
 - However offsets provided to ensure the player is able to jump and reach new ledges even if right on the edge for fun gameplay purposes
-}
groundCollisionCheck :: Float -> (Float, Float) -> GeometryData -> PlayerCollision
groundCollisionCheck playerHeight playerSize geometry
                            | belowTopOfGeometry && horizontalLeftCollision && horizontalRightCollision && bottomCollision = Side
                            | topCollision && horizontalLeftCollision && horizontalRightCollision && bottomCollision = Top
                            | otherwise = None
    where
        geometryXYBound = geometryHorizontalBounds geometry

        -- Checks if the player is below the top line of the geometry - if so they must be colliding on the side
        belowTopOfGeometry = snd (geomLoc geometry) + 0.5 * snd (geomSize geometry) - 0.5 > playerHeight - 0.2 * snd playerSize

        hitboxOffset = 10 -- Offset to allow for last minute jumps and ledge grabs
        -- Simply check if the player is within the bounds of all 4 bounds of the ground rectangle 
        topCollision = snd (geomLoc geometry) + 0.5 * snd (geomSize geometry) + 1 >= playerHeight- 0.5 * snd playerSize
        bottomCollision = snd (geomLoc geometry) - 0.5 * snd (geomSize geometry) <= playerHeight + 0.5 * snd playerSize
        horizontalLeftCollision = fst geometryXYBound - hitboxOffset <= playerStartingPositionX - 0.5 * fst playerSize
        horizontalRightCollision = snd geometryXYBound + hitboxOffset >= playerStartingPositionX + 0.5 * fst playerSize