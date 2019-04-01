# Automaton

A Haskell library for easily defining cellular automata.

 - Quite fast
 - Some helpful functions to generate a random grid, apply game-of-life rules, and more
 - Simple to define a new rule

 ![](eg/gol.gif)

To run an example, clone the repository and install [gifgen](https://github.com/lukechilds/gifgen), then run:

```
cabal exec bash # or zsh, or whatever
rm -r out/*     # only if you've run this already
ghc -O2 gol.hs
./gol           # might take a while. i should probably do some parallel stuff
gifgen -o out.gif -f 25 out/%d.png
```

Or, if you can't be bothered to install gifgen, you could generate a gif with `ffmpeg` or some other tool, or just look at the contents of `out/` yourself. You can try framerates other than 25fps, but with gifgen, it seems that if you set it higher than that, it skips frames when playing it back, which is not ideal.

## Example

This is an example, to show how simple it is to make a game-of-life. Though it's a bit unfair, because game-of-life is actually defined inside the library already:

```haskell
main = do
    board <- random 256 256 (False, True)
    
    forM_ (zip [1..] $ take 1024 $ steps gol board) $ \(i, b) -> do
        img <- renderBoard (option (rgb 0 0 0) (rgb 255 255 255)) b
        savePngFile ("out/" ++ show i ++ ".png") img
        putStrLn $ "finished step " ++ show i ++ " of 1024"
```

If you want to define your own automaton, you will have to make a `Rule a` and a `Palette a`, where `a` is the type of the cells of the grid your automaton will live on. These types are defined as:

```haskell
type Pos = (Int, Int)
type Rule a = Pos -> Board a -> a -> a
type Palette a = a -> Color          -- Color comes from Graphics.GD
```

`Palette a` will only be needed if you want to render your automaton using the built-in rendering functions, which in 99% of cases you will.

And example rule would be `gol`, defined in the library:

```haskell
gol :: Rule Bool
gol pos board True = let n = countNeighbours id board pos in n == 2 || n == 3
gol pos board False = let n = countNeighbours id board pos in n == 3
```

It says that an "alive" cell will remain alive only if it has 2 or 3 neighbours, and a "dead" cell will become alive only if it has 3 neighbours.

A palette for this might look like this:

```haskell
palette :: Palette Bool
palette True = rgb 255 255 255
palette False = rgb 0 0 0
```
