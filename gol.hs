import Automaton
import Graphics.GD
import Control.Monad

main = do
    board <- random 256 256 (False, True)
    
    forM_ (zip [1..] $ take 1024 $ steps gol board) $ \(i, b) -> do
        img <- renderBoard (option (rgb 0 0 0) (rgb 255 255 255)) b
        savePngFile ("out/" ++ show i ++ ".png") img
        putStrLn $ "finished step " ++ show i ++ " of 1024"
    