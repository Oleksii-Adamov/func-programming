

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import Data.IORef

type MyPosition = (GL.GLfloat, GL.GLfloat)
type Velocity = (GL.GLfloat, GL.GLfloat)
type Ball = (MyPosition, Velocity, GL.GLfloat)

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

ballRadius :: GL.GLfloat
ballRadius = 0.1

initialBall1, initialBall2 :: Ball
initialBall1 = ((-0.5, 0), (0.1, 0), ballRadius)
initialBall2 = ((0.5, 0), (-0.1, 0), ballRadius)

main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow "Collision Simulation" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral windowWidth) (fromIntegral windowHeight) }
    _ <- SDL.glCreateContext window
    ballsRef <- newIORef [initialBall1, initialBall2]
    appLoop window ballsRef

appLoop :: SDL.Window -> IORef [Ball] -> IO ()
appLoop window ballsRef = do
    events <- SDL.pollEvents
    let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
    if quit then SDL.destroyWindow window
    else do
        display ballsRef
        SDL.glSwapWindow window
        appLoop window ballsRef

display :: IORef [Ball] -> IO ()
display ballsRef = do
    GL.clear [GL.ColorBuffer]
    GL.preservingMatrix $ do
        ballsList <- readIORef ballsRef
        mapM_ drawBall ballsList
    GL.flush

drawBall :: Ball -> IO ()
drawBall ((x, y), _, r) = do
    renderPrimitive TriangleFan $ do
        color (Color3 1 1 (1 :: GLfloat))
        let slices = 100 :: Int -- Define slices as Int
            twicePi = 2 * pi
            drawCircleVertice :: GLfloat -> IO ()
            drawCircleVertice i =
                let theta = i * twicePi / fromIntegral slices -- Convert slices to GLfloat
                    vx = x + r * cos theta
                    vy = y + r * sin theta
                in vertex $ Vertex2 vx vy
        mapM_ drawCircleVertice [0 .. fromIntegral (slices - 1)] -- Use slices - 1 to avoid using 'fromIntegral' directly
    flush

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

update :: IORef [Ball] -> IdleCallback
update ballsRef = do
    ballsList <- readIORef ballsRef
    let updatedBalls = moveBalls ballsList
    writeIORef ballsRef updatedBalls
    postRedisplay Nothing

moveBalls :: [Ball] -> [Ball]
moveBalls [] = []
moveBalls (ball:rest) = moveBall ball : moveBalls rest
    where
        moveBall ((x, y), (vx, vy), r) =
            let newX = x + vx
                newY = y + vy
                (newVX, newVY) = handleCollision ((newX, newY), (vx, vy), r)
            in ((newX, newY), (newVX, newVY), r)

handleCollision :: Ball -> Velocity
handleCollision ((x, y), (vx, vy), r)
    | x + r >= 1 || x - r <= -1 = (-vx, vy)
    | y + r >= 1 || y - r <= -1 = (vx, -vy)
    | otherwise = (vx, vy)